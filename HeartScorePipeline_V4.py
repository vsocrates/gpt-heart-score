import os
import re
import json
import pandas as pd 
import numpy as np
import openai
import humanize 
from tenacity import (
    retry,
    stop_after_attempt,
    wait_random_exponential,
)  # for exponential backoff
import tiktoken
from dotenv import load_dotenv

load_dotenv()

import os
import openai

DEV_TEST = True
TRIALS = 25
DEV_N_PATIENTS = 10
MAX_INPUT_TOKENS = 125000
GPT_TEMPERATURE = 0.5
# GPT_ENGINE = "decile-gpt-35-turbo-16k"
GPT_ENGINE = "decile-gpt-4-128K"
# 0-indexed
PROMPT_ITERATION = 2

if DEV_TEST:
    TRIALS = 1

################################################################################
################################################################################

### (1) Gather relevant notes 

# Current All Note Types
# Current EKGs
# Current/Prior All Note Types

################################################################################

# (1a.) Current note: Gather time stamp, note type, and provider type, for all 
#         notes related to the current encounter

# prior_notes: these are all the notes associated with a patient. There are no encounter_IDs for the encounters these note are from so we have to order the notes by time and select the most recent one. => PAT_ENC_CSN_ID, ArrivalInstant, visit_occurrence_id, _CreationInstant, Type, TEXT
# ekg_notes: these are all EKGs from a particular CPC encounter. => PAT_ENC_CSN_ID, ORD_VALUE, Comment, RESULT_TIME | ORD_VALUE = was the ECG abnormal or not, Comment = ECG read description
# cpc_notes: these are the notes from a particular CPC encounter. => PAT_ENC_CSN_ID, visit_occurrence_id, ArrivalInstant, person_id, Type, TEXT | note_id == person_id

# DeID is just a unique identifier for all patients or encounters; it can be used to right notes for a specific CPC encounter (including patient-level PMH) across all the datasets

# Read the csv spreadsheet into a dataframe called "allNotes"
prior_notes = pd.read_csv("/Users/vsocrates/Documents/Yale/Heart_Score/NOTES_PRIOR_IDENTIFIED_postprocessed.csv")
ekg_notes = pd.read_csv("/Users/vsocrates/Documents/Yale/Heart_Score/EKG_HEART_IDENTIFIED_postprocessed.csv")
# we are only using CPC notes to define our cohort and not passing them to GPT, since they contain the gold standard HEART score and may lead to data leakage
# cpc_notes = pd.read_csv("/Users/vsocrates/Documents/Yale/Heart_Score/NOTES_CPC_IDENTIFIED_postprocessed.csv")
cpc_notes = pd.read_csv(f"/Users/vsocrates/Documents/Yale/Heart_Score/NOTES_CPC_IDENTIFIED_postprocessed_{PROMPT_ITERATION}.csv")

prior_notes = prior_notes.rename({
                                  "PAT_ENC_CSN_ID":"CPC_PAT_ENC_CSN_ID",
                                  "Document_Time":"_CreationInstant"}, axis=1)
cpc_notes = cpc_notes.rename({"note_id":"DeID"}, axis=1)


# Cast to datetime
prior_notes["_CreationInstant"] = pd.to_datetime(prior_notes['_CreationInstant']).dt.tz_localize(None)
ekg_notes["RESULT_TIME"] = pd.to_datetime(ekg_notes['RESULT_TIME']).dt.tz_localize(None)
cpc_notes["ArrivalInstant"] = pd.to_datetime(cpc_notes['ArrivalInstant']).dt.tz_localize(None)

# we need the ArrivalInstant to the CPC Encounter because we can't use the _CreationInstant to need to create a time delta
prior_notes = prior_notes.merge(cpc_notes[['DeID', "ArrivalInstant"]], on="DeID").rename({"ArrivalInstant":"CPC_ArrivalInstant"}, axis=1)
prior_notes['TIME_SINCE_CPC_ARRIVAL'] = (prior_notes['CPC_ArrivalInstant'] - prior_notes['_CreationInstant']).apply(humanize.naturaldelta)

ekg_notes = ekg_notes.merge(cpc_notes[['DeID', "ArrivalInstant"]], on="DeID").rename({"ArrivalInstant":"CPC_ArrivalInstant"}, axis=1)
ekg_notes['TIME_SINCE_CPC_ARRIVAL'] = (ekg_notes['CPC_ArrivalInstant'] - ekg_notes['RESULT_TIME']).apply(humanize.naturaldelta)

# we create compiledText in the method below, except for cpc_notes, which we don't use below so we do it here
cpc_notes['compiledText'] = ("ED Arrival Time: " + cpc_notes["ArrivalInstant"].astype(str) + 
                                           "\nType: " + "Chest Pain Center Note" +
                                           "\n\n\n" + cpc_notes["deid_text"])

# Filter down so that there's only unique rows (i.e. no duplicates)
prior_notes = prior_notes.drop_duplicates()
ekg_notes = ekg_notes.drop_duplicates()
cpc_notes = cpc_notes.drop_duplicates()

# sort notes by ENC_ID and date/time
prior_notes = prior_notes.sort_values(["DeID", "_CreationInstant"], ascending=False)
ekg_notes = ekg_notes.sort_values(["DeID", "RESULT_TIME"], ascending=True)

# create note index within each patient
prior_notes['note_num'] = prior_notes.groupby("DeID").cumcount()+1
ekg_notes['note_num'] = ekg_notes.groupby("DeID").cumcount()+1

# To get the tokeniser corresponding to a specific model in the OpenAI API:
encoding = tiktoken.encoding_for_model("gpt-4")

def get_notes_by_enc_ID(row):
    
    # get all prior notes associated with a CPC encounter
    prior_notes_row = prior_notes[(prior_notes['DeID'] == row['DeID'])]
    
    # just confirm that they're sorted and get most recent note
    prior_notes_row = prior_notes_row.sort_values("_CreationInstant", ascending=False)
    current_note = prior_notes_row.iloc[0]

    # if the most recent "prior" note isn't an ED provider note or it was created before the current encounter, we drop those patients
    if (current_note['Type'] != "ED Provider Notes") or (current_note['_CreationInstant'] < row['ArrivalInstant']):
        return np.nan, np.nan, np.nan
    
    previous_prior_notes_row = prior_notes_row.iloc[1:]
    
    # 3/4/2024: we want to also drop all EKGs that are after the CPC note was created
    # 3/21/2024: we were having some issues with dates, so we just took the first one
    current_ekg_notes_row = ekg_notes[(ekg_notes['DeID'] == row['DeID'])]
    current_ekg_notes_row = current_ekg_notes_row.sort_values("RESULT_TIME", ascending=True).iloc[[0]]

    current_note_txt = ("#####################################\nCURRENT ED PROVIDER NOTE: \n\n\n" +
                                           "\nType: " + current_note["Type"] + 
                                           "\nProvider: " + ("N/A" if pd.isnull(current_note["Service"]) else current_note["Service"]) + 
                                           "\n\n\n" + current_note["deid_text"]
                        )

    if prior_notes_row.shape[0] > 1:
        previous_prior_notes_row['compiledText'] = ("#####################################\nPAST ENCOUNTER NOTE #" + (previous_prior_notes_row['note_num'] - 1).astype(str) + 
                                "\n\n\n" + "Time Before ED Arrival: " + previous_prior_notes_row["TIME_SINCE_CPC_ARRIVAL"] + 
                                           "\nType: " + previous_prior_notes_row["Type"] + 
                                           "\nProvider: " + previous_prior_notes_row["Service"].fillna("N/A") + 
                                           "\n\n\n" + previous_prior_notes_row["deid_text"]
                                )

    # we don't have person_IDs for EKGs so we can't use prior EKGs from before the CPC encounter
    # previous_ekg_notes_row['compiledText'] = ("#####################################\nEKG NOTE #" + previous_ekg_notes_row['note_num'].astype(str) + 
    #                         "\n\n\n" + previous_ekg_notes_row['compiledText']
    #                         )

    current_ekg_notes_row['compiledText'] = ("#####################################\nEKG IMPRESSION NOTE [#" + current_ekg_notes_row['note_num'].astype(str) + 
                            "] IN CURRENT ENCOUNTER\n\n\n" + "Time Since ED Arrival: " + current_ekg_notes_row["TIME_SINCE_CPC_ARRIVAL"] + 
                                           "\nType: " + "EKG" + 
                                           "\n\n\n" + current_ekg_notes_row["deid_text"]
                            )

    if prior_notes_row.shape[0] > 1:
        previous_prior_notes_txt = "\n\n\n".join(previous_prior_notes_row['compiledText'].tolist())
    else:
        previous_prior_notes_txt = ""

    current_ekg_notes_txt = "\n\n\n".join(current_ekg_notes_row['compiledText'].tolist())

    ## we need 3 things for the HEART Score so we create them below
    # History, Age, Troponin: Current Encounter Note (ED Provider Notes)
    # EKGs: All Current EKGs
    # Risk Factors: Current/Prior All Note Types

    # we don't have person_IDs for EKGs so we can't use prior EKGs from before the CPC encounter
    # all_notes_txt = previous_prior_notes_txt + "\n\n\n" + previous_ekg_notes_txt + "\n\n\n" + current_prior_notes_txt + "\n\n\n" + current_ekg_notes_txt
    all_notes_txt = current_note_txt + "\n\n\n" + current_ekg_notes_txt + "\n\n\n" + previous_prior_notes_txt 

    # we have to truncate to the context window allowable by the GPT model
    current_note_txt = encoding.decode(encoding.encode(current_note_txt)[:MAX_INPUT_TOKENS])
    current_ekg_notes_txt = encoding.decode(encoding.encode(current_ekg_notes_txt)[:MAX_INPUT_TOKENS])
    all_notes_txt = encoding.decode(encoding.encode(all_notes_txt)[:MAX_INPUT_TOKENS])

    return current_note_txt, current_ekg_notes_txt, all_notes_txt


gpt_input = cpc_notes.apply(get_notes_by_enc_ID, axis=1, result_type="expand")
gpt_input = gpt_input.rename({0:"Current_Note", 
                              1:"Current_EKG",
                              2:"All_Notes"}, axis=1)

cpc_notes_processed = pd.concat([cpc_notes, gpt_input], axis=1)

# dropping the ones that don't have any text because they didn't have the right notes
print(f"Before drop missing patients size: {cpc_notes_processed.shape[0]}")
cpc_notes_processed = cpc_notes_processed[~cpc_notes_processed['Current_Note'].isna()]
print(f"After drop missing patients size: {cpc_notes_processed.shape[0]}")

# 224 patients dropped
# 79 don't have the most recent previous encounter note be ED Provider Note
# 145 have the most recent ED Provider Note from before this current encounter's ArrivalInstant
# Final cohort = 1160 - (145 + 79)

################################################################################
################################################################################

### (2) Run GPT pipeline independently 

################################################################################

prompts = pd.read_csv("/Users/vsocrates/Documents/Yale/Heart_Score/prompt_iteration_for_GPT.csv")

system_role_prompt = prompts[prompts['Step'] == "INSTRUCTIONAL_PHRASE"]['Prompt'].squeeze()
age_prompt = prompts[prompts['Step'] == "Age"]['Prompt'].squeeze()
history_prompt = prompts[prompts['Step'] == "History"]['Prompt'].squeeze()
ekg_prompt = prompts[prompts['Step'] == "EKG"]['Prompt'].squeeze()
risks_prompt = prompts[prompts['Step'] == "Risk_Factors"]['Prompt'].squeeze()
onepass_prompt = prompts[prompts['Step'] == "OnePass_Prompt"]['Prompt'].squeeze()

print(os.getenv("AZURE_OPENAI_KEY"))
print(os.getenv("AZURE_OPENAI_ENDPOINT"))
openai.api_type = "azure"
openai.api_version = "2023-07-01-preview"
openai.api_key = os.getenv("AZURE_OPENAI_KEY")
openai.api_base = os.getenv("AZURE_OPENAI_ENDPOINT")


@retry(wait=wait_random_exponential(min=1, max=60), stop=stop_after_attempt(6))
def completion_with_backoff(**kwargs):
    return openai.ChatCompletion.create(**kwargs)


def get_classification_from_completion(completion, last_only):
   matches = list(re.finditer(r"\[.+?\]", completion))
   if matches:
      if last_only:
         bracketed_info = matches[-1].group(0)
         return bracketed_info
      else:
         # get all bracketed answers
         return [match.group(0) for match in matches]
         
   else:
       return "ERROR - No bracketed phrase"

def get_subscore_from_completion(completion):
    bracketed_info = get_classification_from_completion(completion, last_only=True)
    return get_subscore_from_bracketed_answer(bracketed_info)

    
def get_subscore_from_bracketed_answer(bracketed_info):
    if re.search(r"Not enough information", bracketed_info, re.IGNORECASE):
        return "ERROR - Not enough info"
    if bracketed_info.startswith("ERROR"):
        return "ERROR - No bracketed phrase"       

    subscore_match = re.search(r"\((\d)\)", bracketed_info)
    try:
        return float(subscore_match.group(1))
    # TypeError: can't cast to float, AttributeError: doesn't have any groups matched
    except (AttributeError, TypeError) as error:
        return "ERROR - No number found"
    
        #    raise Exception(f"Could not cast subscore as an int: {subscore_match.group(1)}")

def get_onepass_subscores_from_completion(completion):
    bracketed_infos = get_classification_from_completion(completion, last_only=False)

    if type(bracketed_infos) == str:
        return ["ERROR - No bracketed phrase","ERROR - No bracketed phrase","ERROR - No bracketed phrase"]

    history_subscore = "ERROR History Subscore - No number found"
    ekg_subscore = "ERROR EKG Subscore - No number found"
    risks_subscore = "ERROR Risks Subscore - No number found"    
    age_subscore = "ERROR Age Subscore - No number found"    

    # even if we have more than 3 bracketed sections, by looping over all of them, we get the latest (closest to end of GPT answer) bracketed response per subscore
    for bracketed_info in bracketed_infos:
        if re.search(r"\[History", bracketed_info, re.IGNORECASE):
            history_subscore = get_subscore_from_bracketed_answer(bracketed_info)
        elif re.search(r"\[EKG", bracketed_info, re.IGNORECASE):
            ekg_subscore = get_subscore_from_bracketed_answer(bracketed_info)
        elif re.search(r"\[Risk", bracketed_info, re.IGNORECASE):
            risks_subscore = get_subscore_from_bracketed_answer(bracketed_info)
        elif re.search(r"\[Age", bracketed_info, re.IGNORECASE):
            age_subscore = get_subscore_from_bracketed_answer(bracketed_info)

    return history_subscore, ekg_subscore, age_subscore, risks_subscore

# completion = completion_with_backoff(engine="decile-gpt-35-turbo-16k",
#                                      messages = message_text,
#                                     #   temperature=0.7,
#                                     #   max_tokens=800,
#                                     #   top_p=0.95,
#                                     #   frequency_penalty=0,
#                                     #   presence_penalty=0,
#                                     #   stop=None
#                                     )

history_df = []
age_df = []
ekg_df = []
risks_df = []
onepass_df = []

if DEV_TEST:
   cpc_notes_processed = cpc_notes_processed.sample(DEV_N_PATIENTS)

for idx, (_, row) in enumerate(cpc_notes_processed.iterrows()):
    print(idx)
    # history
    history_message_text = [{"role":"system","content":system_role_prompt},
                    {"role":"user","content":history_prompt[:history_prompt.rfind("{")] + row['Current_Note'] + '"""'}]
    # age
    age_message_text = [{"role":"system","content":system_role_prompt},
                    {"role":"user","content":age_prompt[:age_prompt.rfind("{")] + row['Current_Note'] + '"""'}]
    # ekg
    ekg_message_text = [{"role":"system","content":system_role_prompt},
                    {"role":"user","content":ekg_prompt[:ekg_prompt.rfind("{")] + row['Current_EKG'] + '"""'}]
    # risk factors
    risks_message_text = [{"role":"system","content":system_role_prompt},
                    {"role":"user","content":risks_prompt[:risks_prompt.rfind("{")] + row['All_Notes'] + '"""'}]
    # one pass
    onepass_message_text = [{"role":"system","content":system_role_prompt},
                    {"role":"user","content":onepass_prompt[:onepass_prompt.rfind("{")] + row['All_Notes'] + '"""'}]

    # individual subscores
    for category, df, prompt in zip(["history", "age", "ekg", "risk_factors"],
                            [history_df, age_df, ekg_df, risks_df],
                            [history_message_text, age_message_text, ekg_message_text, risks_message_text]):
        for trial in range(TRIALS):
            output = {}
            completion = completion_with_backoff(engine=GPT_ENGINE,
                                    messages=prompt,
                                    temperature=GPT_TEMPERATURE,
                                    )

            output['DeID'] = row['DeID']
            output['completion'] = json.dumps(completion)
            output['completion_text'] = completion['choices'][0]['message']['content']
            output['attempt_number'] = trial
            output['section'] = category
            output['subscore'] = get_subscore_from_completion(completion['choices'][0]['message']['content'])

            df.append(output)
    
    print("Completed individual subscores", flush=True)
    print("Completed onepass subscores...", flush=True)
    # Now do onepass prompts
    for trial in range(TRIALS):
        output = {}
        completion = completion_with_backoff(engine=GPT_ENGINE,
                                messages=onepass_message_text,
                                temperature=GPT_TEMPERATURE,)
        
        output['DeID'] = row['DeID']
        output['completion'] = json.dumps(completion)
        output['completion_text'] = completion['choices'][0]['message']['content']
        output['attempt_number'] = trial
        output['section'] = "onepass"
        history_subscore, ekg_subscore, age_subscore, risks_subscore = get_onepass_subscores_from_completion(completion['choices'][0]['message']['content'])
        output['history_subscore'] = history_subscore
        output['ekg_subscore'] = ekg_subscore
        output['age_subscore'] = age_subscore
        output['risks_subscore'] = risks_subscore

        onepass_df.append(output)


history_df = pd.DataFrame.from_records(history_df)
age_df = pd.DataFrame.from_records(age_df)
ekg_df = pd.DataFrame.from_records(ekg_df)
risks_df = pd.DataFrame.from_records(risks_df)
onepass_df = pd.DataFrame.from_records(onepass_df)


cpc_notes_processed.to_csv(f"/Users/vsocrates/Documents/Yale/Heart_Score/output/cpc_notes_with_gpt_input_{str(PROMPT_ITERATION + 1)}.csv")
history_df.to_csv(f"/Users/vsocrates/Documents/Yale/Heart_Score/output/history_sample_{str(PROMPT_ITERATION + 1)}.csv")
age_df.to_csv(f"/Users/vsocrates/Documents/Yale/Heart_Score/output/age_sample_{str(PROMPT_ITERATION + 1)}.csv")
ekg_df.to_csv(f"/Users/vsocrates/Documents/Yale/Heart_Score/output/ekg_sample_{str(PROMPT_ITERATION + 1)}.csv")
risks_df.to_csv(f"/Users/vsocrates/Documents/Yale/Heart_Score/output/risks_sample_{str(PROMPT_ITERATION + 1)}.csv")
onepass_df.to_csv(f"/Users/vsocrates/Documents/Yale/Heart_Score/output/onepass_sample_{str(PROMPT_ITERATION + 1)}.csv")
