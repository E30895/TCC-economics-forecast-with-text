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

def create_dataset():
  return pd.DataFrame(columns = ['Data', 'endereco'])


def all_days_since(since: str):
    """
    Creates a list of dates starting from a given date up to today.

    Args:
        since (str): The starting date in the format '%Y-%m-%d'.

    Returns:
        pd.DatetimeIndex: A list of dates from the start date to today.
    """
    return pd.date_range(start=since, end=datetime.datetime.today().strftime('%Y-%m-%d'))




def get_request_page(q: str, date: str, source: str):
  
    """
    Requests a search URL from the specified source (e.g., 'g1' or 'valor econômico') and returns the request object 
    if the status_code is 200.

    Args:
        q (str): The search query.
        date (str): The date to filter news articles.
        source (str): The source of the news articles (e.g., 'g1', 'valor').

    Returns:
        tuple: A tuple containing the request object and the search query if successful, 
               or (False, q) if an error occurs.
    """

    url = ""

    if source.lower() == "g1":
        url = f'https://g1.globo.com/busca/?q={q}&page=1&order=relevant&species=notícias&from={date}T00%3A00%3A00-0300&to={date}T23%3A59%3A59-0300'

    elif source.lower() == "valor":
        url = f'https://valor.globo.com/busca/?q={q}&page=1&order=relevant&species=notícias&from={date}T00%3A00%3A00-0300&to={date}T23%3A59%3A59-0300'
    
    else:
        print(f"Source '{source}' não reconhecido.")
        return False, q

    try:
        req = requests.get(url, timeout= 60.0)

        if req.status_code == 200:
            return req, q

    except Exception as e:
        err_name = type(e).__name__
        print(f'Requisition error for URL {url}: {err_name}')
        return False, q
    



def get_page(response):
  soup = BeautifulSoup(response.text, 'html.parser')
  return soup.select('.widget--info__text-container a')



def initialize_g1(start: str, source: str, q):

  dataset = create_dataset()
  days = all_days_since(start)

  for i, day in enumerate(days):
    dataset.to_excel(f'dataset_{q}_bkp.xlsx', encoding='latin-1', index=False) if i % 7 == 0 else None
    print(day)

    try:
      response, q = get_request_page(q = q, date = str(day.date()), source = source)
      page = get_page(response = response)

      for http in page:
        
        try:
          http = http.get('href')
          dataset.loc[len(dataset)] = [day,  http]

        except Exception as e1:
          err_name = type(e1).__name__
          print(f'Nãoo achou o link {http}: {err_name}')
          continue

    except Exception as e2:
        err_name = type(e2).__name__
        print(f'Requisition error for URL {http}: {err_name}')
    
  dataset['endereco'] = "https:" + dataset['endereco']
  dataset.to_excel(f'dataset_{q}.xlsx')
    
  return dataset, q


def get_http_text(dataset, q):
  textos_completos = []

  https = dataset['endereco'].to_list()
  restantes = 0
  
  for i, http in enumerate(https):
    print(f"Restam {len(https)-restantes} consultas")
    sessao = requests.Session()
    
    try:
      url = f'{http}'
      req = sessao.get(url, timeout= 15.0)
      soup = BeautifulSoup(req.text, 'html.parser')
      body_element = soup.body
      paragrafos = [p.get_text(separator='\n', strip=True) for p in body_element.find_all('p')]
      texto_completo = '\n'.join(paragrafos)

      textos_completos.append(texto_completo)
      pd.DataFrame(textos_completos).to_excel(f'textos_completos_{q}.xlsx', encoding='latin-1', index=False) if i % 100 == 0 else None
      restantes += 1
      sessao.close()

    except TimeoutError as e1:
       texto_completo = "N/D"
       textos_completos.append(texto_completo)
       pd.DataFrame(textos_completos).to_excel(f'textos_completos_{q}.xlsx', encoding='latin-1', index=False) if i % 100 == 0 else None
       print(f"Erro {e1}, aguardando {time.sleep(600)} segundos")
       sessao.close()
       continue
  
    #EM CASO DE REDIRECIONAMENTO DE LINK
    except AttributeError as e2:
        
        try:
            script_redirecionamento = soup.find('script', string=re.compile(r'window\.location\.replace\("(.+)"\)'))
            match = re.search(r'window\.location\.replace\("(.+)"\)', str(script_redirecionamento))
            url_redirecionada = match.group(1)

            print(f'{e2} redirecionado para {url_redirecionada}')
            
            req = requests.get(url_redirecionada, timeout= 300.0)
            soup = BeautifulSoup(req.text, 'html.parser')
            body_element = soup.body

            paragrafos = [p.get_text(separator='\n', strip=True) for p in body_element.find_all('p')]
            texto_completo = '\n'.join(paragrafos)
            
            textos_completos.append(texto_completo)
            pd.DataFrame(textos_completos).to_excel(f'textos_completos_{q}.xlsx', encoding='latin-1', index=False) if i % 100 == 0 else None
            restantes += 1
            sessao.close()

        except TimeoutError as e3:
            texto_completo = "N/D"
            textos_completos.append(texto_completo)
            pd.DataFrame(textos_completos).to_excel(f'textos_completos_{q}.xlsx', encoding='latin-1', index=False) if i % 100 == 0 else None
            sessao.close()
            print(f"Erro {e3}, aguardando {time.sleep(600)} segundos")
            continue
            
        except Exception as e4:
            texto_completo = "N/D"
            textos_completos.append(texto_completo)
            pd.DataFrame(textos_completos).to_excel(f'textos_completos_{q}.xlsx', encoding='latin-1', index=False) if i % 100 == 0 else None
            print(f"Erro ao obter URL redirecionada {url_redirecionada}: {e4}")
            time.sleep(600)
            restantes +=1
            sessao.close()
            continue  # Continua para a próxima iteração em caso de erro na URL redirecionada 

    except Exception as e:
      texto_completo = "N/D"
      textos_completos.append(texto_completo)
      pd.DataFrame(textos_completos).to_excel(f'textos_completos_{q}.xlsx', encoding='latin-1', index=False) if i % 100 == 0 else None
      print(f"Erro ao obter URL redirecionada: {e}")
      time.sleep(600)
      restantes +=1
      sessao.close()
      continue

  dataset['Texto'] = textos_completos
  dataset.to_excel(f'textos_completos_{q}.xlsx', index = False)

  return dataset
