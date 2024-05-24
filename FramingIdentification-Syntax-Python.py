#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Syntax for  framing identification (thematic frames)

@author: rguevaraa
"""
# Load libraries
import os
import pandas as pd
import numpy as np
from gensim.models import Word2Vec
import re #regular expressions
from collections import Counter

#Set working Directory
os.chdir('/Users/rguevaraa/Documents/Posgrado/Lund-SSDA/Thesis/Data_Collection/Final Data')

#Load data
news = pd.read_csv('A5.df_news_split_LONG_FILTER_cleaned_ParNODU.csv')

# =================================================================================================================
# ######################################### Training Word2Vec model for framing ##########################
# =================================================================================================================

############################# STEP 1: Identify frames with a Word2vec model
#Split paragraphs into words
clean_paragraphs = news['Paragraph_cleaned'].apply(lambda x: str(x) if isinstance(x, str) else '')
split_paragraphs = clean_paragraphs.apply(lambda x: x.split()) # Split paragraphs into words

#Train a Word2Vec model
model = Word2Vec(split_paragraphs, sg=1,  #Method Skip-gram
                 min_count=25, # minimun frequency of a word to be included in the model: 25 times
                 vector_size=300) # size of the vector (vector dimensionality)
# Save the model
#model.save("Model")

model = Word2Vec.load("Model") #Load model

#Similarity of keywords

# FOR Loop 1: Find similarities for ALL of these words and SAVE then in DIFFERENT FILES
words = ['venezolano','venezolanos','venezolana','venezolanas', 'veneco', 'venecos', 'veneca', 'venecas',
            'extranjero','extranjeros','extranjera', 'extranjeras', 'foráneo', 'foráneos',
            'foranea', 'foránea', 'foraneas', 'foráneas', 'migrante', 'migrantes',
            'chamo', 'chamos', 'chama', 'chamas']

for word in words: 
    try: 
        similar_words = model.wv.most_similar(word, topn=30) 
        table = pd.DataFrame(similar_words,columns=['Word', 'Similarity Score']) 
        table = table.sort_values(by='Similarity Score', ascending=False) 
        filename = f'table-{word}.csv' 
        table.to_csv(filename, encoding='utf-8', index=False) 
        print('File {filename} was correctly saved') 
    except KeyError: 
        print(f'Word "{word}" not found in the model vocabulary. Skipping.') 
    except Exception as e:
        print(f'An error occurred with word "{word}": {e}. Skipping.')

# FOR Loop 2:  Store results in one single file
# Empty list to store final results with similarities
final_results = pd.DataFrame()

# Unique similar words across all iterations
unique_words = set()

for word in words:
    try:
        # Most similar words function TOP 30
        similar_words = model.wv.most_similar(word, topn=30)
        
        # Filter out any similar words that have already been processed in the previous request
        filtered_similar_words = [(w, s) for w, s in similar_words if w not in unique_words]
        
        # Update the unique words
        unique_words.update(w for w, s in filtered_similar_words)
        
        # If there are filtered words to process, create a DataFrame
        if filtered_similar_words:
            table = pd.DataFrame(filtered_similar_words, columns=['Word', 'Similarity Score'])
            table['Reference Word'] = word  # Add a column for the reference word
            
            # Append the results to the final dataset
            final_results = pd.concat([final_results, table], ignore_index=True)
    except KeyError:
        print(f'Word "{word}" not found in the model vocabulary. Skipping.')
    except Exception as e:
        print(f'An error occurred with word "{word}": {e}. Skipping.')

# Save a file with ALL SIMILAR WORDS (unique cases, AKA if two words are part of two different words, just report 1)
final_results.to_csv('all_similar_words.csv', encoding='utf-8', index=False)


# Similarity for analyzing gender differences
#Similar words for venezuelan male
similar_venezolano = model.wv.most_similar('venezolano', topn=20)
similar_venezolanos = model.wv.most_similar('venezolanos', topn=20)
similar_chamo = model.wv.most_similar('chamo', topn=20)
similar_chamos = model.wv.most_similar('chamos', topn=20)
similar_veneco = model.wv.most_similar('veneco', topn=20)
similar_venecos = model.wv.most_similar('venecos', topn=20)

# Save it in a dataframe
df_venezolanos = pd.DataFrame({
    'Word_venezolano': [word for word, similarity in similar_venezolano],
    'Similarity_venezolano': [similarity for word, similarity in similar_venezolano],
    'Word_venezolanos': [word for word, similarity in similar_venezolanos],
    'Similarity_venezolanos': [similarity for word, similarity in similar_venezolanos],
    'Word_chamo': [word for word, similarity in similar_chamo],
    'Similarity_chamo': [similarity for word, similarity in similar_chamo],
    'Word_chamos': [word for word, similarity in similar_chamos],
    'Similarity_chamos': [similarity for word, similarity in similar_chamos],
    'Word_veneco': [word for word, similarity in similar_veneco],
    'Similarity_veneco': [similarity for word, similarity in similar_veneco],
    'Word_venecos': [word for word, similarity in similar_venecos],
    'Similarity_venecos': [similarity for word, similarity in similar_venecos]
})

df_venezolanos.to_csv('Similarity_venezolanos.csv')

# Similarity for Venezuelan femlaes
similar_venezolana = model.wv.most_similar('venezolana', topn=20)
similar_venezolanas = model.wv.most_similar('venezolanas', topn=20)
similar_chama = model.wv.most_similar('chama', topn=20)
similar_veneca = model.wv.most_similar('veneca', topn=20)

# Save it in a dataframe
df_venezolanas = pd.DataFrame({
    'Word_venezolana': [word for word, similarity in similar_venezolana],
    'Similarity_venezolana': [similarity for word, similarity in similar_venezolana],
    'Word_venezolanas': [word for word, similarity in similar_venezolanas],
    'Similarity_venezolanas': [similarity for word, similarity in similar_venezolanas],
    'Word_chama': [word for word, similarity in similar_chama],
    'Similarity_chama': [similarity for word, similarity in similar_chama],
    'Word_veneca': [word for word, similarity in similar_veneca],
    'Similarity_veneca': [similarity for word, similarity in similar_veneca],
})

df_venezolanas.to_csv('Similarity_venezolanas.csv')


################################# STEP 2: Define dictionaries per frame ##############

# Define a dictionary per frame
crime_frame_1 = [
    'habido*', 'aducir', 'delincuent*', 'avezad*', 'mortal', 'sindicar', 'parroquiano*', 'hampa*', 'facci[oó]n',
    'agrupaci[óo]n', 'maldito?', 'peligroso', 'cartel', 'delatar', 'v[íi]nculo', 'liderad*', 'alia?', 'autor*',
    'rolex', 'sindicad*', 'raquetero?', 'raqueteo', 'secuestrador', 'cabecilla', 'cae', 'desarticular', 'delincuencial',
    'apodado', 'capturad*', 'capturan', 'perteneciente', 'complicidad', 'integrante?', 'serenos', 'presuntamente', 
    'secuestrad*', 'homicidio?', 'custodio', 'testig*', 'dirincri', 'interrogar', 'secuestros', 'polic[íi]a*', 
    'pesquisa', 'artefacto?', 'contrabando', 'extorsivo', 'retenid*', 'del[íi]to?', 'allanar', 'ilícito', 'armamento?', 
    'facineros*', 'rev[óo]lver', 'guarida', 'c[óo]mplice*', 'fusil*', 'escondido*', 'm[áa]scara', 'asaltante*', 
    'joyería', 'lujos*',
    ] 

migration_frame_1 = [
    'ilegalmente',  'sureñ*',  'inspector*',  'irregular',  'verificaci[óo]n',  'documentaci[óo]n',  'pase', 
    'indocumentad*',  'desplazad*',  'inmigración',  'vulnerable',  'unicef',  'desplazamiento',  'refugiad*',
    'acogida',  'vulnerabilidad',  'integraci[óo]n',  'fronteras',  'acnur',  'tacna',  'misi[óo]n',  'oim', 
    'megaoperativo*',  'onu', 
    ]

sexualviolence_frame_1 = [ 
    'acoso',  'tocamiento*',  'meretriz',  'celo*',  'sexualmente',  'proxeneta*',  'trans',  'proxenetismo', 
    'prostituci[óo]n',  'meretricio',  'ex',  'amoroso',  'f[ée]mina',  'enamorado',  'indebido', 
    ]

violence_frame_1 = [
    'cruel',  'piedad',  'maltratar',  'f[íi]sicamente',  'verbal',  'verbalmente',  'masacrar',  'maltrato?',
    'brutalmente',  'desfigurar',  'agredid*',  'engaño?',  'temid*',  'temible',  'discriminar',  'odio',
    'turba',  'violentamente',  'reducido',  'machete',  'destrozar',  'salvajemente',  'agraviada', 
    'encerrar',  'agresor',  'explotar',  'explotaci[óo]n', 
    ]

drugs_frame_1 = ['narcotr[áa]fico',  'antidrogas',  'tr[áa]fico', ]

minlaw_frame_1 = ['infractor*',  'incumplir',  'evadir',  'infringir',  'exceso', ]

aes_frame_1 =['hermos*',  'guap*',  'lind*',  'bell*',  'look',  'estilo',  'sensual',  'lucir', ]

art_frame_1 = [ 'cantante',  'reality',  'youtuber',  'arte',  'art[íi]stico',  'cultural',  'cultura',
               'canci[óo]n', ]

work_frame_1 = [ 'barbero?',  'anfitriona?',  'vendedora?',  'mesera?',  'mozo?',  'ambulante?',  'minero?',  
                'transportista?', ]


# Define a FUNCTION that identifies the SECOND ROUND of Word2vec
# The aim of the function is to ask for similarity words for each of the words in the frame, and give a df with
# the unique terms that are not part of the frame (erased terms that already appear on the frame)
# This second round will also be qualitatively selected

def similarity_function_2(model,frame_list, filename = None, encoding= None): 
#model =  name of the Word2vec model, #frame_list = list of words with regex
    similar_words = []
    for regex in frame_list:
        # Extract words from the regex expression
        potential_words = [word for word in model.wv.index_to_key if re.match(regex, word)]
        # Get similar words for each word in the framing list 
        for word in potential_words:
            similar = model.wv.most_similar(word, topn=100) #ask for the TOP 100
            similar_words.extend([w[0] for w in similar])

    # Count the occurrences of each similar word
    word_counts = Counter(similar_words)

    filtered_words = [(word, count) for word, count in word_counts.items() if not any(re.match(r.replace('*', '.*').replace('?', '.?'), word) for r in frame_list)]

    # Get the top 50 unique words after filtering words that are already in the framing list
    top_filtered_words = Counter(dict(filtered_words)).most_common(50)

    # Convert to a df
    df_top_filtered_words = pd.DataFrame(top_filtered_words, columns=['Word', 'Count'])
    
    # check if a file name has been created, if thats the case then save it
    if filename:
       df_top_filtered_words.to_csv(filename, encoding=encoding, index=False)
    
    return df_top_filtered_words

# parameters for the fuction: model = word2vec model, fame_list = list of words, filename= specify it, encoding, encoding = utf-8

#Apply function per frame
os.chdir('/Users/rguevaraa/Documents/Posgrado/Lund-SSDA/Thesis/Analysis/Word2Vec/Similarity socres (Round 2)')

similarity_function_2(model, crime_frame_1, 'crime_frame_2ndround.csv', encoding= 'utf-8')
similarity_function_2(model, migration_frame_1,'migration_frame_2ndround.csv', encoding= 'utf-8' )
similarity_function_2(model, sexualviolence_frame_1, 'sexualviolence_frame_2ndround.csv', encoding= 'utf-8')
similarity_function_2(model, violence_frame_1, 'violence_frame_2ndround.csv', encoding= 'utf-8')
similarity_function_2(model, drugs_frame_1, 'drugs_frame_2ndround.csv', encoding= 'utf-8')
similarity_function_2(model, minlaw_frame_1, 'minlaw_frame_2ndround.csv', encoding= 'utf-8')
similarity_function_2(model, aes_frame_1, 'aes_frame_2ndround.csv', encoding= 'utf-8')
similarity_function_2(model, art_frame_1, 'art_frame_2ndround.csv', encoding= 'utf-8')
similarity_function_2(model, work_frame_1, 'work_frame_2ndround.csv', encoding= 'utf-8')

os.chdir('/Users/rguevaraa/Documents/Posgrado/Lund-SSDA/Thesis/Analysis/Word2Vec')

######################################## STEP 3: Define Final framing dictionaries #########################

# Some frames changed based on the manual inspection

aes_frame = [
    'REGEX:hermos.*','REGEX:guap.*','REGEX:lind.*','REGEX:bell.*','REGEX:atractiv.*',
    'look','estilo','sensual','lucir', 
    'increíble','REGEX:bonit.*','feliz','cariño','gustar','disfrutar','amar','modelo',
    ]

entertainment_frame = [
    'cantante','reality','youtuber','arte','REGEX:art(í|i)stico','cultural','cultura',
    'REGEX:canci(ó|o)n','personaje','novela','talento','actriz','película','actor','show',
    'concierto','musical','REGEX:famos.*','artista','interpretar','animar','fan','participante',
    'actor',	
    ]

ilegal_frame = [
    'ilegalmente', 'meretriz','proxeneta','proxenetismo', 'REGEX:prostituci(ó|o)n',
    'meretricio', 'REGEX:narcotr(á|a)fico','antidrogas','REGEX:tr(á|a)fico',
    'REGEX:traficante?', 'contrabando', 'REGEX:comercializaci(ó|o)n', 
    'REGEX:posesi(ó|o)n', 'tenencia', 'REGEX:droga?', 'REGEX:narcotraficante?',
    'REGEX:coca(i|í)na','marihuana', 'REGEX:ligad.*', 'inmerso', 'prostituir',
    'inopinado', 'infringir', 'incumplir', 'infractor', 'evadir', 'burlar',
    'ejerc', 'ilegal', 'allanar', 'REGEX:il(i|í)cito',
    ]

crime_frame = [
    'habido','aducir','REGEX:delincuent.*','avezado','mortal','sindicar','REGEX:parroquiano?',
    'hampa','REGEX:facci(ó|o)n','REGEX:maldito?', 'REGEX:peligroso?', 'cartel',
    'delatar','REGEX:v(í|i)nculo?','liderado', 'REGEX:alia?', 'rolex','sindicado',
    'REGEX:raquetero?','raqueteo','secuestrador','cabecilla','cae','desarticular',
    'REGEX:delincuencia?', 'REGEX:apodad(o|a)','REGEX:capturad.*', 'capturan',
    'perteneciente','complicidad','serenos','presuntamente','REGEX:secuestrado?',
    'REGEX:homicidio?', 'custodio', 'REGEX:testigo?','dirincri','interrogar',
    'REGEX:secuestro?','REGEX:polic(i|í)a?','pesquisa', 'REGEX:artefacto?',
    'extorsivo', 'REGEX:retenido?', 'REGEX:delito?', 'REGEX:armamento?',
    'REGEX:facineros.*', 'REGEX:rev(ó|o)lver', 'guarida','REGEX:c(ó|o)mplice?',
    'fusil','máscara','REGEX:asaltante?','joyería','lujoso','temido',
    'REGEX:sanguinario?','REGEX:malhechor.*','REGEX:tiroteo?', 'REGEX:temible?',
    'feroz', 'estafa', 'REGEX:abatido?','arrestar','REGEX:pr(ó|o)fugo?',
    'REGEX:maleante?', 'REGEX:ratero?', 'perpetrar','REGEX:sicario?', 
    'REGEX:arma?','asesinar','matar','acribillar','detener',
    'robo','robar','policial', 'REGEX:asesinato?', 'REGEX:crimen.*',
    'criminal','pnp','REGEX:polic(í|i)a?', 'REGEX:prisi(ó|o)n.*',
    'asaltar','REGEX:ladr(o|ó)n','asesinado', 'descuartizado',
    'REGEX:detenid(o|a)?', 'requisitoria','alterar', 'REGEX:desaparici(o|ó)n',
    'trans', 'REGEX:temible?','temido', 'engaño',
    ]

migration_frame = [
    'REGEX:inspector.*','irregular','REGEX:verificaci(ó|o)n','REGEX:documentaci(ó|o)n',
    'pase','REGEX:indocumentado?','REGEX:desplazad.*','REGEX:inmigraci(ó|o)n',
    'REGEX:vulnerable?','unicef','desplazamiento','REGEX:refugiad.*','acogida',
    'vulnerabilidad','REGEX:integraci(ó|o)n','REGEX:frontera?','acnur',
    'tacna','REGEX:misi(ó|o)n','oim','REGEX:megaoperativo?','onu', 
    'REGEX:cooperaci(ó|o)n','REGEX:migrant.*','REGEX:amnist(i|í)a','flujo',
    'tacna','REGEX:migraci(ó|o)n','migratorio','permiso','asilo', 'irregularidad',
    'exceso','ptp','permanencia','tramitar','superintendencia','regularizar', 
    ]

sexualviolence_frame = [
    'acoso','REGEX:tocamiento?','REGEX:celo?','sexualmente','REGEX:ex novi(o|a)', 'REGEX:exnovi(o|a)',
    'ex', 'expareja', 'ex pareja','indebido','REGEX:denunciado?','abuso','REGEX:violaci(ó|o)n',
    'violar','REGEX:violador.*','sexual','ultrajar','abusar','acosar',
    'agredida', 'abusador', 'agraviada', 'feminicidio','feminicida', 'callejero',
    'machista',
    ]
   
violence_frame = [
    'maltratar','físicamente','verbal','verbalmente','masacrar','maltrato',
    'brutalmente', 'desfigurar', 'REGEX:agredid.?','odio', 'turba','violentamente',
    'machete','destrozar','salvajemente','encerrar','explotar','explotación',
    'REGEX:puñet.*','REGEX:acuchilla.*','inconsciente','golpeado','empujar',
    'arrancar','patada', 'clavar','ensangrentado','navaja','desfigurado',
    'apuñalar','golpiza','propinar','linchar','forcejeo',
    'agredir','violencia', 'quemar','pegar', 'amenazar', 'amenaza',
    'atacar', 'maniatar', 'agresor',
    ]

work_frame = [
    'barbero','anfitriona','REGEX:vendedor.*','mesera','mozo','ambulante','minero',
    'transportista','ayudante','REGEX:jalador.*','REGEX:emprendedor.*',
    'REGEX:motociclista?','REGEX:caf(é|e)','golosina','REGEX:obrero?',
    'REGEX:mec(á|a)nico?','desempeñar','REGEX:asistent.*','empanada',
    'limpieza','REGEX:ingenier(a|o)','REGEX:limpiaparabrisa?','barbería','grifo',
    'REGEX:comerciante?','tizana','informal','fruta','chifa','arepa',
    'REGEX:trabajador?','esfuerzo', 'trabaja', 'REGEX:oficio?','emprendimiento?'
    ]

######################################### STEP 4: Count Framing word in a paragraph (Paragraph level) ###################

#Define a function to count framing words

def count_frame_words(paragraph, words_to_count):
    if isinstance(paragraph, str):
        regex_patterns = []
        for pattern in words_to_count:
            if pattern.startswith('REGEX:'):
                regex_pattern = pattern[6:]  # Remove 'REGEX:' prefix
                # For patterns ending with .*, do not alter the pattern
                regex_patterns.append(regex_pattern)
            else:
                # Treat as a plain string: escape and compile exact matches as regex
                regex_pattern = re.escape(pattern)
                regex_patterns.append(f'\\b{regex_pattern}\\b')
        
        combined_pattern = '|'.join(regex_patterns)
        # Use re.IGNORECASE for case-insensitive matching
        return len(re.findall(combined_pattern, paragraph, re.IGNORECASE))
    else:
        return 0

# Calculate framing words per paragraph
news['aes_fr'] = news['Paragraph_cleaned'].apply(lambda x: count_frame_words(x, aes_frame))
news['entertainment_fr'] = news['Paragraph_cleaned'].apply(lambda x: count_frame_words(x, entertainment_frame))
news['crime_fr'] = news['Paragraph_cleaned'].apply(lambda x: count_frame_words(x, crime_frame))
news['ilegal_fr'] = news['Paragraph_cleaned'].apply(lambda x: count_frame_words(x, ilegal_frame))
news['migration_fr'] = news['Paragraph_cleaned'].apply(lambda x: count_frame_words(x, migration_frame))
news['sexvi_fr'] = news['Paragraph_cleaned'].apply(lambda x: count_frame_words(x, sexualviolence_frame))
news['violence_fr'] = news['Paragraph_cleaned'].apply(lambda x: count_frame_words(x, violence_frame))
news['work_fr'] = news['Paragraph_cleaned'].apply(lambda x: count_frame_words(x, work_frame))

#Calculate a framing coefficient per paragraph (Total framed words in paragraph / Total number of words in the paragraph)
news['aes_coef_par'] = news['aes_fr']/news['text_length_words_paragraph']
news['entertainment_coef_par'] = news['entertainment_fr']/news['text_length_words_paragraph']
news['crime_coef_par'] = news['crime_fr']/news['text_length_words_paragraph']
news['ilegal_coef_par'] = news['ilegal_fr']/news['text_length_words_paragraph']
news['migration_coef_par'] = news['migration_fr']/news['text_length_words_paragraph']
news['sexvi_coef_par'] = news['sexvi_fr']/news['text_length_words_paragraph']
news['violence_coef_par'] = news['violence_fr']/news['text_length_words_paragraph']
news['work_coef_par'] = news['work_fr']/news['text_length_words_paragraph']

# Descriptives of the framing coefficient (paragraph level)
news['crime_coef_par'].describe()
news['aes_coef_par'].describe()
news['entertainment_coef_par'].describe()
news['ilegal_coef_par'].describe()
news['migration_coef_par'].describe()
news['sexvi_coef_par'].describe()
news['violence_coef_par'].describe()
news['work_coef_par'].describe()

#################################### STEP 5: Aggregate data to the article level #######################


### Define levels of agreggation
count_columns = ['text_lenght_words','aes_fr','entertainment_fr','crime_fr','ilegal_fr', 
                 'migration_fr','sexvi_fr', 'violence_fr', 'work_fr']

frames_columns = ['crime_coef_par', 'aes_coef_par', 'entertainment_coef_par', 'ilegal_coef_par',
                  'migration_coef_par', 'sexvi_coef_par', 'violence_coef_par',
                  'work_coef_par']

aggregations = {} # create an empty dictionary

#FOR loop: Define different types of aggregation
for col in news.columns:
    if col in frames_columns:
        aggregations[col] = 'mean'
    elif col in count_columns:
        aggregations[col] = 'sum'
    elif col != 'index_news':
        aggregations[col] = 'first'

#Agreggate data
news_aggregated = news.groupby('index_news').agg(aggregations).reset_index()


############################### STEP 6: Create framing coefficient (Article's level) ###############

# Create coefficients based on TOTAL FRAMED WORDS (article) / TOTAL WORDS IN ARTICLE
# Calculate a FRAMING COEFFICIENT PER ARTICLE (article's level)
news_aggregated['aes_coef'] = news_aggregated['aes_fr']/news_aggregated['Full_text_length']
news_aggregated['entertainment_coef'] = news_aggregated['entertainment_fr']/news_aggregated['Full_text_length']
news_aggregated['crime_coef'] = news_aggregated['crime_fr']/news_aggregated['Full_text_length']
news_aggregated['ilegal_coef'] = news_aggregated['ilegal_fr']/news_aggregated['Full_text_length']
news_aggregated['migration_coef'] = news_aggregated['migration_fr']/news_aggregated['Full_text_length']
news_aggregated['sexvi_coef'] = news_aggregated['sexvi_fr']/news_aggregated['Full_text_length']
news_aggregated['violence_coef'] = news_aggregated['violence_fr']/news_aggregated['Full_text_length']
news_aggregated['work_coef'] = news_aggregated['work_fr']/news_aggregated['Full_text_length']

# Descriptives
news_aggregated['crime_coef'].describe()
news_aggregated['aes_coef'].describe()
news_aggregated['entertainment_coef'].describe()
news_aggregated['ilegal_coef'].describe()
news_aggregated['migration_coef'].describe()
news_aggregated['sexvi_coef'].describe()
news_aggregated['violence_coef'].describe()
news_aggregated['work_coef'].describe()


# CODE INTO DUMMY VARIABLE
# If the framing coefficient (article's level) is higher than 0.005 code as TRUE (frame is presented)
news_aggregated['aes_frame_dummy'] = news_aggregated['aes_coef'] > 0.005
news_aggregated['crime_frame_dummy'] = news_aggregated['crime_coef'] > 0.005
news_aggregated['ilegal_frame_dummy'] = news_aggregated['ilegal_coef'] > 0.005
news_aggregated['entertainment_frame_dummy'] = news_aggregated['entertainment_coef'] > 0.005
news_aggregated['migration_frame_dummy'] = news_aggregated['migration_coef'] > 0.005
news_aggregated['sexvi_frame_dummy'] = news_aggregated['sexvi_coef'] > 0.005
news_aggregated['violence_frame_dummy'] = news_aggregated['violence_coef'] > 0.005
news_aggregated['work_frame_dummy'] = news_aggregated['work_coef'] > 0.005

# Count values of the dummy varibales
news_aggregated['aes_frame_dummy'].value_counts()
news_aggregated['crime_frame_dummy'].value_counts()
news_aggregated['ilegal_frame_dummy'].value_counts()
news_aggregated['entertainment_frame_dummy'].value_counts()
news_aggregated['migration_frame_dummy'].value_counts()
news_aggregated['sexvi_frame_dummy'].value_counts()
news_aggregated['violence_frame_dummy'].value_counts()
news_aggregated['work_frame_dummy'].value_counts()

#create a frame value for articles with NO FRAMES identified
news_aggregated['no_frame'] = ~(news_aggregated['aes_frame_dummy'] 
                                | news_aggregated['crime_frame_dummy'] 
                                | news_aggregated['ilegal_frame_dummy'] 
                                | news_aggregated['work_frame_dummy'] 
                                | news_aggregated['entertainment_frame_dummy'] 
                                | news_aggregated['migration_frame_dummy']
                                | news_aggregated['sexvi_frame_dummy'] 
                                | news_aggregated['violence_frame_dummy'])

news_aggregated['no_frame'].value_counts()

####################### Step 7: Add a variable that identifies gender ###############################

keywords_gender = ['chama', 'chamas', 'veneca','venecas','venezolanas', 'extranjeras']
keywords_gender = '|'.join(keywords_gender) # 7246 cases

news_aggregated['if_female_full'] = news_aggregated['Full_text'].str.contains(keywords_gender, case=False, na=False) #case false is case insentive, doesnt care about capitalized words
news_aggregated['if_female_full'].value_counts() #

news_aggregated['if_female_clean'] = news_aggregated['Full_text_cleaned'].str.contains(keywords_gender, case=False, na=False) #case false is case insentive, doesnt care about capitalized words
news_aggregated['if_female_clean'].value_counts() #

#Save file
news_aggregated.to_csv('News-Coded-2.csv')

