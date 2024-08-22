import pandas as pd
import numpy as np
import requests
import time
import datetime
import re
import string
import matplotlib.pyplot as plt
import nltk
from textblob import TextBlob
from deep_translator import GoogleTranslator
from nltk.corpus import stopwords
from nltk.stem import SnowballStemmer
from nltk.tokenize import word_tokenize
from bs4 import BeautifulSoup




def drop_duplicate_news(dataset):
    dataset= dataset.groupby('Texto', as_index=False).first()
    dataset = dataset.sort_values(by=['Data'])
    dataset = dataset[['Data', 'endereco', 'Texto', 'Num_Palavras-Chave']]
    return dataset




def remove_useless(dataset, q, min_crit=1):

    if q == 'Agronegócio':
        palavras_chave = ['Agronegócio',
                          'Agricultura',
                          'Pecuária',
                          'Agroindústria',
                          'Produção Agrícola',
                          'Cultivo',
                          'Plantio',
                          'Colheita',
                          'Agropecuária',
                          'Gestão Rural',
                          'Tecnologia no Campo',
                          'Irrigação',
                          'Fertilizantes',
                          'Defensivos Agrícolas',
                          'Mercado Agrícola',
                          'Exportação Agrícola',
                          'Logística Agrícola',
                          'Sustentabilidade no Agronegócio',
                          'Agrobusiness',
                          'Cadeia Produtiva']

    elif q == 'Indústria':
        palavras_chave = ['Indústria',
                          'Produção',
                          'Manufatura',
                          'Fábrica',
                          'Produção Industrial',
                          'Processo Industrial',
                          'Tecnologia Industrial',
                          'Automatização',
                          'Máquinas',
                          'Equipamentos Industriais',
                          'Engenharia Industrial',
                          'Logística',
                          'Supply Chain',
                          'Qualidade',
                          'Padrões Industriais',
                          'Operações',
                          'Eficiência',
                          'Inovação Industrial',
                          'Sustentabilidade Industrial',
                          'Energia Industrial']
    
    elif q == 'Mercado de Trabalho':
        palavras_chave = ['Mercado de Trabalho',
                          'Emprego',
                          'Desemprego',
                          'Salário',
                          'Vagas',
                          'Recrutamento',
                          'Carreira',
                          'Oportunidades',
                          'RH',
                          'Entrevista',
                          'Currículo',
                          'Contratação',
                          'Estágio',
                          'Desenvolvimento Profissional',
                          'Competências',
                          'Treinamento',
                          'Benefícios',
                          'Trabalho Remoto',
                          'Flexibilidade',
                          'Empregabilidade']
            
    elif q == 'Mercado Financeiro':
        palavras_chave = ['Mercado Financeiro', 
                          'Mercado de Capitais', 
                          'Bolsa de Valores', 
                          'IBOV',
                          'índice Bovespa', 
                          'Câmbio' 
                          'Ações', 
                          'Corretora', 
                          'Economia', 
                          'Finanças',
                          'Tesouro Direto',
                          'Derivativos', 
                          'Análise Financeira', 
                          'Capital', 
                          'Dividendos']
    
    elif q == 'Serviços':
        palavras_chave = ['Setor de Serviços',
                          'Serviços',
                          'Atendimento ao Cliente',
                          'Consultoria',
                          'Tecnologia da Informação',
                          'TI',
                          'Software',
                          'SaaS (Software as a Service)',
                          'Infraestrutura',
                          'Logística',
                          'Transporte',
                          'Telecomunicações',
                          'E-commerce',
                          'Turismo',
                          'Hotelaria',
                          'Restaurante',
                          'Educação',
                          'Saúde',
                          'Financeiro',
                          'Seguros',
                          'Gestão de Projetos']

    dataset['Texto'] = dataset['Texto'].astype(str)
    
    # Função para contar o número de palavras-chave encontradas em cada texto
    def count_keywords(text):
        return sum(keyword.lower() in text.lower() for keyword in palavras_chave)
    
    # Criar uma nova coluna com o número de palavras-chave encontradas para cada texto
    dataset['Num_Palavras-Chave'] = dataset['Texto'].apply(count_keywords)
    
    # Aplicar a máscara para manter apenas os textos relevantes
    mask = dataset['Num_Palavras-Chave'] >= min_crit
    dataset = dataset[mask]
    
    dataset.reset_index(drop=True, inplace=True)
    
    return dataset

def translate(textos):

    lista_textos = list(textos['Texto'])
    list_tradução = []

    for texto in lista_textos:

        try:
            print(f'Caracteres Texto: {len(texto)}')
            texto_traduzido = TextBlob(texto).translate(from_lang="pt", to='en')
            list_tradução.append(texto_traduzido)
            print('Sucesso na primeira tentativa')
        
        except:

            try:
                print(f'Caracteres Texto: {len(texto)}')
                print("A primeira tentativa não foi. Segunda tentativa em 1 minuto")
                time.sleep(60)
                texto_traduzido = TextBlob(texto).translate(from_lang="pt", to='en')
                list_tradução.append(texto_traduzido)
                print("Sucesso na segunda tentativa")

            except:
                print("Erro na tradução, pulando para o proximo")
                texto_traduzido = "ERRO"
                list_tradução.append(texto_traduzido)
                
                
    textos['Traducao'] = list_tradução
    textos['Traducao'] = textos['Traducao'].map(lambda x:str(x))

    return textos


def remove_stopwords_br(text):
    stop_words_pt = set(stopwords.words('portuguese'))    
    tokens = word_tokenize(text)
    filtered_tokens = [word for word in tokens if word.lower() not in stop_words_pt]
    return ' '.join(filtered_tokens)




def remove_stopwords_en(text):
    stop_words_pt = set(stopwords.words('english'))    
    tokens = word_tokenize(text)
    filtered_tokens = [word for word in tokens if word.lower() not in stop_words_pt]
    return ' '.join(filtered_tokens)




def remove_pontuaiton(text):
    punctuation = set(string.punctuation)
    for i in text:
        if i in punctuation:
            text = text.replace(i, "")
            
    return text




def remove_numbers(text):
    string = text
    return ''.join(filter(lambda z: not z.isdigit(), string))



def remove_expressoes(text):
    text = re.sub(r"\n", " ", text)
    text = re.sub(r'\r', " ", text)
    text = re.sub(r'-', ' ', text)
    text = re.sub(r'\t', ' ', text)
    text = re.sub(r'\s+', ' ', text)

    return text




def tratamento(dataset, q):
    print("Iniciando Tratamento")
    dataset = remove_useless(dataset, q)
    dataset = drop_duplicate_news(dataset)
    dataset.loc[:, 'Texto'] = dataset['Texto'].map(remove_expressoes)
    
    dataset = translate(dataset)

    dataset['Texto'] = dataset['Texto'].map(lambda x:str(x).lower())
    dataset['Texto'] = dataset['Texto'].map(remove_stopwords_br)
    dataset['Texto'] = dataset['Texto'].map(remove_stopwords_en)
    dataset['Texto'] = dataset['Texto'].map(remove_pontuaiton)
    dataset['Texto'] = dataset['Texto'].map(remove_numbers)

    dataset.to_excel(f'noticicas_tratadas_{q}.xlsx', index = False)
    return dataset