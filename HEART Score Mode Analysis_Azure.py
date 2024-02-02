# # HEART Score Subscore-based Mode Analysis
# 
# Compared to the one-pass HEART score method, in which GPT-3.5-turbo is queried to retrieve all subscores at once and return each subscore and total score, this method aims to evaluate each subscore a multitude of times. GPT will be queried 10 times for each subscore, and the most common output or mode of the subscore will be the subscore used for the final HEART score generated.

import openai
import os


!source /etc/environment


# # Test


import os
import openai
openai.api_type = "azure"
openai.api_version = "2023-05-15" 
openai.api_key = "c43177c64dc54430bdcca64be4141810" #os.getenv("AZURE_OPENAI_KEY")
openai.api_base = "https://decile-chatgpt-test.openai.azure.com/" #os.getenv("AZURE_OPENAI_ENDPOINT") # your endpoint should look like the following https://YOUR_RESOURCE_NAME.openai.azure.com/
deployment_name = "decile-heart-score-gpt35-16k"


response = openai.ChatCompletion.create(
    engine=deployment_name, # The deployment name you chose when you deployed the GPT-35-Turbo or GPT-4 model.
    messages=[
        {"role": "system", "content": "Assistant is a large language model trained by OpenAI."},
        {"role": "user", "content": "Who were the founders of Microsoft?"}
    ]
)

# print(response)

# print(response['choices'][0]['message']['content'])


# ### Reading in all encounter notes from one pre-compiled txt document


# Open the file in read mode
with open('Test1doc.txt', 'r') as file:
    # Read the contents of the file into a variable
    file_contents = file.read()

# Print the contents of the file
print(file_contents)


def isolateString(ch1,ch2,s):
    return s[s.find(ch1)+1:s.find(ch2)]

# %% [markdown]
# ### Mode Analysis for History Subscore
# #### Repeated GPT Analysis of Note #1 for history subscore 10 times. Then, display mode of history subscore to ideally improve accuracy and precision of subscore.
# 
# Based on all of the patient's notes below, and considering the patient's symptoms and past medical conditions, please choose between the following options to rate their history as: [Slightly suspicious (0)] [Moderately suspicious (1)] [Highly suspicious (2)] [Not enough information to determine History score (9)] Please provide your answer in brackets by choosing between the answers above (e.g. [Slightly suspicious (0)] ). No Prose.
# 


historyList = []
prompt = "Based on all of the patient's notes below, and considering the patient's symptoms and past medical conditions, please choose between the following options to rate their history as: [Slightly suspicious (0)] [Moderately suspicious (1)] [Highly suspicious (2)] [Not enough information to determine History score (9)] Please provide your answer in brackets by choosing between the answers above (e.g. [Slightly suspicious (0)] ). No Prose."
for x in range(0, 9):
    messages = [{"role": "user", "content": (prompt + file_contents)}]
    response = openai.ChatCompletion.create(engine=deployment_name, messages=messages)
    for i, choice in enumerate(response["choices"], start=1):
        phrase = choice["message"]["content"]
        for ch in phrase:
            if ch.isnumeric():
                historyList.append(ch)
        print(choice["message"]["content"])



modeHistory = max(set(historyList), key=historyList.count)
print(modeHistory)

# %% [markdown]
# ### Mode Analysis for EKG Subscore
# #### Repeated GPT Analysis of Note #1 for EKG subscore 10 times. Then, display mode of EKG subscore to ideally improve accuracy and precision of subscore.
# 
# Please consider the patient's EKG findings. Would you categorize it as: [Normal (0)] [Non-specific repolarization disturbance (1)] [Significant ST deviation (2)] [Not enough information to determine EKG score (9)] Additional information: 1 point: No ST deviation but LBBB, LVH, repolarization changes (e.g. digoxin); 2 points: ST deviation not due to LBBB, LVH, or digoxin. Please provide your choice between the answers above in brackets  (e.g. [Normal (0)] ). No Prose
# 


ekgList = []
prompt = "Please consider the patient's EKG findings. Would you categorize it as: [Normal (0)] [Non-specific repolarization disturbance (1)] [Significant ST deviation (2)] [Not enough information to determine EKG score (9)] Additional information: 1 point: No ST deviation but LBBB, LVH, repolarization changes (e.g. digoxin); 2 points: ST deviation not due to LBBB, LVH, or digoxin. Please provide your choice between the answers above in brackets  (e.g. [Normal (0)] ). No Prose"
for x in range(0, 9):
    messages = [{"role": "user", "content": (prompt + file_contents)}]
    response = openai.ChatCompletion.create(engine=deployment_name, messages=messages)
    for i, choice in enumerate(response["choices"], start=1):
        phrase = choice["message"]["content"]
        for ch in phrase:
            if ch.isnumeric():
                ekgList.append(ch)
        print(choice["message"]["content"])
modeEKG = max(set(ekgList), key=ekgList.count)
print("Mode EKG:", modeEKG)

# %% [markdown]
# ### Mode Analysis for Age Subscore
# #### Repeated GPT Analysis of Note #1 for Age subscore 10 times. Then, display mode of Age subscore to ideally improve accuracy and precision of subscore.
# 
# Based on the encounter note log below, how would you classify the patient's age: 
# [<45 (0)] [45-64 (1)] [>= 65 (2)]  [Not enough information to determine Age score]
# 
# Please provide your choice between the answers above in brackets (e.g. [<45 (0)] ). No Prose.
# 


ageList = []
def isolateString(ch1,ch2,s):
    return s[s.find(ch1)+1:s.find(ch2)]
prompt = "Based on the encounter note log below, how would you classify the patient's age: [<45 (0)] [45-64 (1)] [>= 65 (2)]  [Not enough information to determine Age score] Please provide your choice between the answers above in brackets (e.g. [<45 (0)] ). No Prose."
for x in range(0, 9):
    messages = [{"role": "user", "content": (prompt + file_contents)}]
    response = openai.ChatCompletion.create(engine=deployment_name, messages=messages)
    for i, choice in enumerate(response["choices"], start=1):
        phrase = choice["message"]["content"]
        ageList.append(isolateString("(", ")", phrase))
        print(choice["message"]["content"])
modeAge = max(set(ageList), key=ageList.count)
print("Mode Age:", modeAge)

# %% [markdown]
# ### Mode Analysis for Risk Factors Subscore
# #### Repeated GPT Analysis of Note #1 for Risk Factors subscore 10 times. Then, display mode of Risk Factors subscore to ideally improve accuracy and precision of subscore.
# 
# Consider the following risk factors: HTN, hypercholesterolemia, DM, obesity (BMI >30 kg/m²), smoking (current, or smoking cessation ≤3 mo), positive family history (parent or sibling with CVD before age 65); atherosclerotic disease: prior MI, PCI/CABG, CVA/TIA, or peripheral arterial disease. 
# 
# For the patient below, across all the combined encounter notes, how many risk factors are present? 
# [No known risk factors (0)] [1-2 risk factors (1)] [>= 3 risk factors or history of atherosclerotic disease (2)] [Not enough information to determine Risk Factor score (9)]
# 
# Please provide your final choice, listing one option between the answers above in brackets (e.g. [No known risk factors (0)] ). No Prose.
# 


riskList = []
prompt = "Consider the following risk factors: HTN, hypercholesterolemia, DM, obesity (BMI >30 kg/m²), smoking (current, or smoking cessation ≤3 mo), positive family history (parent or sibling with CVD before age 65); atherosclerotic disease: prior MI, PCI/CABG, CVA/TIA, or peripheral arterial disease. For the patient below, across all the combined encounter notes, how many risk factors are present? [No known risk factors (0)] [1-2 risk factors (1)] [>= 3 risk factors or history of atherosclerotic disease (2)] [Not enough information to determine Risk Factor score (9)] Please provide your final choice, listing one option between the answers above in brackets (e.g. [No known risk factors (0)] ). No Prose."
for x in range(0, 9):
    messages = [{"role": "user", "content": (prompt + file_contents)}]
    response = openai.ChatCompletion.create(engine=deployment_name, messages=messages)
    for i, choice in enumerate(response["choices"], start=1):
        phrase = choice["message"]["content"]
        for ch in phrase:
            if ch.isnumeric():
                riskList.append(ch)
        print(choice["message"]["content"])
modeRisk = max(set(riskList), key=riskList.count)
print("Mode Risk:", modeRisk)

# %% [markdown]
# ### Mode Analysis for Troponins Subscore
# #### Repeated GPT Analysis of Note #1 for Troponins subscore 10 times. Then, display mode of Troponins subscore to ideally improve accuracy and precision of subscore.
# 
# Find the troponin value in based on careful review of all of the encounter note logs provided below. Note that the troponins may be listed without a unit beside it. 
# 
# How would you categorize assessment of the patient's initial troponin measurement:[=< normal limit (0)] [1–3x normal limit (1)] [>3x normal limit (2)] [Not enough information to determine Troponin score (9)] 
# 
# The normal limit for high sensitivity troponin according to Yale New Haven Health is < 12 ng/L. 
# 
# Please provide your choice between the answers above in brackets (e.g. [=< normal limit (0)] ). No Prose.
# 
# 
# 


tropList = []
prompt = "Find the troponin value based on careful review of all of the encounter note logs provided below. Note that the troponins may be listed without a unit beside it. How would you categorize assessment of the patient's initial troponin measurement:[=< normal limit (0)] [1–3x normal limit (1)] [>3x normal limit (2)] [Not enough information to determine Troponin score (9)] The normal limit for high sensitivity troponin according to Yale New Haven Health is < 12 ng/L. Please provide your choice between the answers above in brackets (e.g. [=< normal limit (0)] ). No Prose. "
for x in range(0, 9):
    messages = [{"role": "user", "content": (prompt + file_contents)}]
    response = openai.ChatCompletion.create(engine=deployment_name, messages=messages)
    for i, choice in enumerate(response["choices"], start=1):
        phrase = choice["message"]["content"]
        tropList.append(isolateString("(", ")", phrase))
        print(choice["message"]["content"])
modeTrop = max(set(tropList), key=tropList.count)
print("Mode Troponin:", modeTrop)

# %% [markdown]
# ### Final HEART Score
# The final HEART Score after summing each of the individual mode analysis of the subscore prompts.


totalHeart = int(modeHistory) + int(modeEKG) + int(modeAge) + int(modeRisk) + int(modeTrop)
print("Total Heart Score:", totalHeart)
# should be # 2 + 1 + 2 + 2 + 2 = 9?
# hmm I got 2 + 2 + 2 + 3 + 2 = 11? 
# EKG and Risk Factors are different. 






