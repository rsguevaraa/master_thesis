#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Cleaning syntax

@author: rguevaraa
"""

# Load libraries
import os #chage directory
import pandas as pd #manipulate df
import re #regular expressions
import unicodedata #library to deal with encoding
import spacy #spacy to clean data
#spacy.cli.download("es_core_news_lg") #spacy functions trained on long data

# Setting working directory to load data
os.chdir('/Users/rguevaraa/Documents/Posgrado/Lund-SSDA/Thesis/Data_Collection/Final_df')

# Load data (2 dataframes, one with links and headings, the other with summary and body)
df_links_2017_2023 = pd.read_csv('df_links_2017-2023.csv') #Links of the news and heading
df_corpus_2017_2023 = pd.read_csv('corpus_2017-2023.csv') # Summary + Body Summary of the article

#Set the new working directory to save the progress
os.chdir('/Users/rguevaraa/Documents/Posgrado/Lund-SSDA/Thesis/Data_Collection/Final Data')

############################ PHASE 1: MERGING BOTH DATASETS############################################

################################ Cleaning 1: Erase duplicated cases######################
df_corpus_2017_2023['Duplicated']= df_corpus_2017_2023.duplicated(subset=['News ID'])
df_corpus_2017_2023['Duplicated'].value_counts() #count duplicated cases n=88 880
df_corpus_2017_2023 = df_corpus_2017_2023.drop_duplicates(subset=['News ID']) #erase duplicated cases

#df_corpus_2017_2023.to_csv('1.df_corpus_2017_2023_NODUPLICATES.csv', index=False) # new df with no duplicates

## Join both df
df_links_2017_2023 = df_links_2017_2023.rename(columns={'Link ID': 'News ID'}) #rename Key

df_merge_2017_2023 = df_links_2017_2023.join(df_corpus_2017_2023.set_index('News ID'),
                                             on='News ID',
                                             lsuffix='_links', 
                                             rsuffix='_corpus')

####################################Cleaning 1: Erase empty cases #############################
# Erase the unnecesary columns
df_merge_2017_2023.drop(columns=['Unnamed: 0_corpus','Duplicated'], inplace=True)

# Define empty cases
df_merge_2017_2023['Body-empty'] = df_merge_2017_2023['Body Text'].str.len() > 0 #cases with empty body
df_merge_2017_2023['Summary-empty'] = df_merge_2017_2023['Summary'].str.len() > 0 #cases with empty summary

df_merge_2017_2023['Body-empty'].value_counts() #461 cases
df_merge_2017_2023['Summary-empty'].value_counts() #617 cases

#define subsets to explore the empty cases
no_summary = df_merge_2017_2023[df_merge_2017_2023['Summary-empty'] == False] #empty summary (n= 617)
no_body = df_merge_2017_2023[df_merge_2017_2023['Body-empty'] == False] #empty body (n=461)
no_summary_body = df_merge_2017_2023[(df_merge_2017_2023['Summary-empty'] == False) & (df_merge_2017_2023['Body-empty'] == False)] #empty body and summary (n=376)

#erase cases
df_merge_clean_1723 = df_merge_2017_2023[~((df_merge_2017_2023['Summary-empty'] == False) & (df_merge_2017_2023['Body-empty'] == False))] #empty body ad empty summary
#len(df_merge_clean_1723) - len(df_merge_2017_2023) #corroborate 376 cases erased

df_merge_clean_1723 = df_merge_2017_2023[~(df_merge_2017_2023['Body-empty'] == False)] # No body
#df_merge_clean_1723['Body-empty'].value_counts() # 0 cases left

#to maintain the opinion articles
df_merge_clean_1723['opinion'] = df_merge_clean_1723['News ID'].str.contains('/opinion/')
no_summary = df_merge_clean_1723[df_merge_clean_1723['Summary-empty'] == False] #empty summary
df_merge_clean_1723 = df_merge_clean_1723[~((df_merge_clean_1723['Summary-empty'] == False) & (df_merge_clean_1723['opinion'] == False))] #empty body ad empty summary

#corroborate if opinions withouth summary remain
empty = df_merge_clean_1723[df_merge_clean_1723['Summary-empty']== False] #all 180 empty cases withouth summary are opinions, so it worked

# Erase innecesary variables
df_merge_clean_1723.drop(columns=['Summary-empty','Body-empty','opinion'], inplace=True)

#df_merge_clean_1723.to_csv('2.df_57318.csv', index = False) # Save momentarily

################################ Cleaning 2: Filtering Data Frame by keywords ############################################# 
# Load data in the last step
df_merge_clean_1723= pd.read_csv('2.df_57318.csv')

#Keywords for filtering articles. Just if they contain any of this terms.
keywords = ['venezolano','venezolanos','venezolana','venezolanas', 'veneco', 'venecos', 'veneca', 'venecas',
            'extranjero','extranjeros','extranjera', 'extranjeras', 'foraneo', 'foráneo', 'foraneos', 'foráneos',
            'foranea', 'foránea', 'foraneas', 'foráneas', 'migrante', 'migrantes',
            'chamo', 'chamos', 'chama', 'chamas']
keywords = '|'.join(keywords)

# Compile the regular expression with the IGNORECASE flag
df_merge_clean_1723['if_heading_contain'] = df_merge_clean_1723['Heading'].str.contains(keywords, case=False, na=False) #case false is case insentive, doesnt care about capitalized words

#Check if SUMMARY contains of one the keywords:
df_merge_clean_1723['if_summary_contain'] = df_merge_clean_1723['Summary'].str.contains(keywords, case=False, na=False) #case false is case insentive, doesnt care about capitalized words

#Check if BODY Text contains one of the keywords:
df_merge_clean_1723['if_body_contain'] = df_merge_clean_1723['Body Text'].str.contains(keywords, case=False, na=False) #case false is case insentive, doesnt care about capitalized words

#Check if LINK contains one of the keywords:
df_merge_clean_1723['if_link_contain'] = df_merge_clean_1723['News ID'].str.contains(keywords, case=False, na=False) #case false is case insentive, doesnt care about capitalized words

#create a new df by erasing cases withouth content in heading/summary/body/link
filtered_df = df_merge_clean_1723[~((df_merge_clean_1723['if_heading_contain']==False)&
                             (df_merge_clean_1723['if_summary_contain']==False)&
                             (df_merge_clean_1723['if_body_contain']==False)&
                             (df_merge_clean_1723['if_link_contain']==False))]
#len(df_merge_clean_1723) -len(filtered_df) #24 627 cases were erased

#filtered_df.to_csv('3.df_32691.csv', index = False) #Save the process

#df = pd.read_csv('3.df_32463.csv')
df = filtered_df

# Extract the "category" from the newspaper
split_url = df['News ID'].str.split('/', expand = True) # split the Link into the different parts, is basically extracting the "/"
df['Auto_Category'] = split_url[1] #Add category as variable in the df
df.drop(columns=['Summary-normal', 'Headline-normal'], inplace=True)


#df.to_csv('3.df_32463.csv', index = False)#re-save the df in same file bc its adding one mo


##############################Cleaning 3: FILTERING BY AUTO-CATEGORIES #########################

# Table with AUTO-Categories
Auto_category_table = df['Auto_Category'].value_counts() # Table 
Auto_category_table=pd.DataFrame(Auto_category_table) # transform it to a df
#Auto_category_table.to_csv('Auto_category_table.csv')

# Create a new dataset only with SELECTED Categories
df_selected = df[((df['Auto_Category'] == "edicion")|
                  (df['Auto_Category'] == "actualidad")|
                  (df['Auto_Category'] == "opinion")|
                  (df['Auto_Category'] == "politica")|
                  (df['Auto_Category'] == "lima")|
                  (df['Auto_Category'] == "policial")|
                  (df['Auto_Category'] == "peru")|
                  (df['Auto_Category'] == "mujer")|
                  (df['Auto_Category'] == "regionales")|
                  (df['Auto_Category'] == "ciudad")|
                  (df['Auto_Category'] == "viral")|
                  (df['Auto_Category'] == "miscelanea")|
                  (df['Auto_Category'] == "columnistas")|
                  (df['Auto_Category'] == "investigacion")|
                  (df['Auto_Category'] == "cultura"))]

#df_selected.to_csv('4.df_18165_15categories.csv') #save df
#df_selected = pd.read_csv('4.df_18043_15categories.csv')
#df_selected.drop(columns=['Body-normal'], inplace=True)


########################## Cleaning 4: Join paragraphs and erase Outliers with too much text
df_selected['Full_text'] = df_selected['Heading'] + '/' + df_selected['Summary'] + '/' + df_selected['Body Text']
df_selected['Text_length'] = df_selected['Full_text'].apply(lambda x: len(str(x))) #measure lenght of each article

# Identify outlier cases with too much text 
threshold = 15000  # Set a threshold for very long articles

# Select articles above the threshold length
selected_articles = df_selected[df_selected['Text_length'] > threshold]

# Define a list of selected cases to drop (based on the index of the df)
cases_to_drop = [2358, 2382, 6088, 6452, 8797,8830,8916,10131,11376,13316,16437,22780,23347,24540, 
                 24591,24716,25396,25460,25667,25739,26317,26338,26388,30918,37201,37212,37261,37525, 
                 38789,41872,50485,50567,50809,52400,53519,53654]

df_selected_NO = df_selected.drop(cases_to_drop) #Erase problematic cases
#selected_articles_2 = df_selected_NO[df_selected_NO['Text_length'] > threshold] #test if they were erased properly and just mantain the other ones

#Plot lenght of characters
import matplotlib.pyplot as plt
plt.hist(df_selected_NO['Text_length'], bins=30, color='skyblue', edgecolor='black')
plt.xlabel('Length of Characters')
plt.ylabel('Frequency')
plt.title('Histogram of Character Lengths')
plt.show()

df_selected_NO.reset_index(inplace = True)
df_selected_NO.rename(columns={'index': 'index_global'}, inplace=True) 
df_selected_NO.drop(columns=['Unnamed: 0_links', 'ID_1'], inplace=True)
#df_selected_NO.to_csv('4.1.df_18129_NOOUTLIERS.csv')

############################## Cleaning 5: Divide dataset in News and Opinions ##########################
# Create 2 datasets
# Load data
df_selected = pd.read_csv('4.1.df_18129_NOOUTLIERS.csv')

################################## DATAFRAME A: Create datasetf for news-articles
news = df_selected[~(((df_selected['Auto_Category'] == "opinion") | 
                        (df_selected['Auto_Category'] == "columnistas")))]
news.rename(columns={'Unnamed: 0': 'index_news'}, inplace=True)

# news.to_csv('A.df_news_16624.csv')

################################ DATAFRAME B: FOR OPINIONS
opinions = df_selected[((df_selected['Auto_Category'] == "opinion") | 
                        (df_selected['Auto_Category'] == "columnistas"))]

#erase innecesary index and create a new index
opinions.drop(columns=['Unnamed: 0'], inplace=True)
opinions.reset_index(inplace = True)
opinions.rename(columns={'index': 'index_opinions'}, inplace=True)  # Rename the index column to 'variable'

# opinions.to_csv('B.df_opiniones_1505.csv', encoding = 'utf-8')

###################################### Cleaning 6: Divide dataframe into PARAGRAPHS#############
# Define a cleaning function to change S/. for SOLES (currency)
def currency_function (text):
    # Replace currency symbols with the name of the currency
    text = text.replace("S/.", "SOLES")
    text = text.replace("S/", "SOLES")
    return text
#Apply the function
news['Full_text_normal'] = news['Full_text'].apply(lambda x: currency_function(x))

# Split text into paragraphs
split_articles = news['Full_text_normal'].str.split(r'/{1,2}', expand = True)
# Replace empty spaces with NA
split_articles.replace('', None, inplace=True)
#Merge both datasets
news_split = pd.concat([news, split_articles], axis=1) #From 23, to 122 columns

#Cleaned function  (Load from the next section when cleaning the syntax just do it)
news_split['Full_text_cleaned'] = news_split['Full_text'].apply(lambda x: cleaning_function(x))

#Function to count words per paragraph
def count_words_paragraph(text):
    if isinstance(text, str):
        return len(text.split())
    else:
        return 0

#This is for counting the total number of words per document
news_split['Full_text_lenght'] = news_split['Full_text_cleaned'].apply(count_words_paragraph)
news_split.drop('Full_text_normal', axis=1, inplace=True) #Erase the text_full_normal

#news_split.to_csv('A1.df_news_16624-Paragraph.csv')#save the df

############################### Cleaning 7: News in Paragraph (LONG FORMAT)###########
# select variables that are no paragraphs
id_vars = [col for col in news_split.columns if not str(col).isdigit()]
# Transform df into a long format
long_df = pd.melt(news_split, id_vars=id_vars, value_name='Paragraph', var_name='Paragraph_Number')
#Erase NA's
long_df = long_df.dropna(subset=['Paragraph'])
#reset index
long_df.reset_index(drop=False, inplace=True)
long_df.rename(columns={'index': 'ID_paragraph_original'}, inplace=True)

#Save
#long_df.to_csv('A2.df_news_split_LONG.csv') #169 177 rows


############################### Cleaning 8: Paragraphs with only venezuelans ###########
venezuelan_keywords = [r'\bextranjer[oa]s?\b', r'\bforáne[ao]s?\b', r'\bvenezolan[ao]s?\b',
                       r'\bforane[ao]s?\b',r'\bmigrantes?\b',r'\bvenec[ao]s?\b',
                       r'\bcham[ao]s?\b']
OR_pattern = '|'.join(venezuelan_keywords)
regex = re.compile(OR_pattern, re.IGNORECASE)

long_df['if_venezuelan'] = long_df['Paragraph'].str.contains(regex, na=False)
#long_df['if_venezuelan'].value_counts()
#long_df.drop(columns=['if_venezuelan1','if_venezuelan2'], inplace=True)

#Filter if the paragraph contain the keywords
long_df_OV = long_df[long_df['if_venezuelan']== True] #50 254 rows
#long_df_OV['if_venezuelan'].value_counts()
#long_df_OV.to_csv('A3.df_news_split_LONG_FILTER.csv', encoding='utf-8')
#long_df_OV = pd.read_csv('A3.df_news_split_LONG_FILTER.csv')

################################## Cleaning 9 : Spacy cleaning #####################
nlp = spacy.load("es_core_news_lg") # Load large size newspaper trained model

# Check if 'buenos' is in the stop words list and remove it
if 'buenos' in nlp.Defaults.stop_words:
    nlp.Defaults.stop_words.remove('buenos')
    nlp.vocab['buenos'].is_stop = False 

# Define a cleaning function 
# tokenization, lemmatization, lowercasing, no stopwords, reomoving punctuation and Name entity recognition
def cleaning_function(doc):
    # Convert floats to strings
    if isinstance(doc, float):
        doc = str(doc)
    doc = doc.lower() #lowercasing before preprocessing everything..So it does not conflict with the NER
    
    processed_doc = nlp(doc) # begin the nlp function
    
    # Compile the patterns of regular expresions for words to ignore
    patterns = [
        re.compile(r'polic[ií]a.*', re.IGNORECASE),
        re.compile(r'venezolan.*', re.IGNORECASE), 
        re.compile(r'migrante.*', re.IGNORECASE),
        re.compile(r'for[aá]ne.*', re.IGNORECASE),
        re.compile(r'extranjer.*', re.IGNORECASE),
        re.compile(r'venec.*', re.IGNORECASE),
        re.compile(r'cham.*', re.IGNORECASE)
    ]
    
    # Define a set of specific words to exclude
    words_to_exclude = {'tú', 'sjm'}
    
    #Define words to include
    words_to_include = {'buenos'}

    tokens = []
    for token in processed_doc:
        token_text = token.lemma_.lower()  # Preprocess token text
        
        #Check if the word "buenos" is presented, if it include it
        if token_text in words_to_include:
            tokens.append(token_text)
            continue  # Skip further checks for this token
        
        if token.is_stop or not token.is_alpha or token_text in words_to_exclude:
            continue  # Skip stopwords, non-alpha, and excluded words
        # Check if the token matches any of the regex patterns
        if any(pattern.match(token.text) for pattern in patterns):
            tokens.append(token_text)
        # Filter out 'PERSON' or 'LOCATION' entities not matching the patterns
        elif token.ent_type_ in ['PER', 'LOC']:
            continue
        else:
            tokens.append(token_text)
    return ' '.join(tokens)

#Apply cleaning function for paragraphs
long_df_OV['Paragraph_cleaned'] = long_df_OV['Paragraph'].apply(lambda x: cleaning_function(x))
long_df_OV=long_df_OV.sort_values(by=['index_news', 'Paragraph_Number'])

# Define a function that counts the words per paragraph based on the cleaned paragraph (spacy)
def count_words_paragraph(text):
    if isinstance(text, str):
        return len(text.split())
    else:
        return 0

long_df_OV['text_length_words_paragraph'] = long_df_OV['Paragraph_cleaned'].apply(count_words_paragraph)

#drop innecesary variables
long_df_OV.drop(columns=['if_body_contain1','if_body_contain2','if_body_contain',
                         'if_heading_contain','if_heading_contain1','if_heading_contain2',
                         'if_summary_contain','if_link_contain'], inplace = True)

#long_df_OV = long_df_OV.rename(columns={'text_lenght_words': 'text_length_words'}) # old name --> new name

#save
#long_df_OV.to_csv('A4.df_news_split_LONG_FILTER_cleaned.csv', encoding= 'utf-8')

############# Cleaning 10. Erase duplicated paragraphs and Erase cases with only 1 word

#Erase paragraphs with only one word
long_df_OV_clean = long_df_OV[long_df_OV['text_length_words_paragraph'] > 1]

long_df_OV['Duplicated_paragraph']= long_df_OV.duplicated(subset=['Paragraph'])
long_df_OV['Duplicated_paragraph'].value_counts() #count duplicated cases n= 7 592
long_df_OV_clean.drop_duplicates(subset=['Paragraph'], inplace=True) # 42 637 cases instead of 50 223

#Drop innecesary variables
long_df_OV_clean.drop(columns=['Duplicated_paragraph'],inplace = True)
long_df_OV_clean.drop(columns=['Unnamed: 0.1'],inplace = True)

# Save dataframe
long_df_OV_clean.to_csv('A5.df_news_split_LONG_FILTER_cleaned_ParNODU.csv', encoding= 'utf-8')

#print(long_df_OV_clean.columns)




