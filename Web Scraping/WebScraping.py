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


def all_days_since(since: str) -> pd.DatetimeIndex:
    """
    Cria uma lista de datas contendo todos os dias desde uma data específica até hoje.
    
    Parâmetros:
    - since (str): Data de início no formato '%Y-%m-%d'.

    Retorna:
    - pd.DatetimeIndex: Objeto DatetimeIndex contendo todas as datas desde a data especificada até a data atual.
    """
    return pd.date_range(start=since, end=datetime.datetime.today().strftime('%Y-%m-%d'))


def get_request_page(q: str, date: str, source: str) -> tuple:
    """
    Faz uma solicitação à URL de pesquisa de uma fonte especificada (por exemplo, 'g1' ou 'valor econômico') 
    e retorna o objeto de resposta da requisição se o status_code for igual a 200.

    Parâmetros:
    - q (str): Termo de pesquisa.
    - date (str): Data da pesquisa no formato '%Y-%m-%d'.
    - source (str): Fonte da notícia ('g1' ou 'valor').

    Retorna:
    - tuple: Uma tupla contendo o objeto de resposta (requests.Response) e o termo de pesquisa (str). 
      Caso a fonte não seja reconhecida ou ocorra um erro na requisição, retorna (False, str).
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
        req = requests.get(url, timeout=60.0)

        if req.status_code == 200:
            return req, q

    except Exception as e:
        err_name = type(e).__name__
        print(f'Requisition error for URL {url}: {err_name}')
        return False, q


def get_page(response: requests.Response) -> list:
    """
    Extrai os links das notícias contidos na página de resposta da requisição.

    Parâmetros:
    - response (requests.Response): Objeto de resposta da requisição.

    Retorna:
    - list: Lista de elementos de link (tags <a>) que contêm as URLs das notícias.
    """
    soup = BeautifulSoup(response.text, 'html.parser')
    return soup.select('.widget--info__text-container a')


def initialize_g1(start: str, source: str, q: str) -> tuple:
    """
    Inicializa a busca de notícias na fonte 'g1' ou 'valor' desde uma data inicial e armazena os resultados em um dataset.

    Parâmetros:
    - start (str): Data de início no formato '%Y-%m-%d'.
    - source (str): Fonte da notícia ('g1' ou 'valor').
    - q (str): Termo de pesquisa.

    Retorna:
    - tuple: Uma tupla contendo o dataset atualizado (pd.DataFrame) e o termo de pesquisa (str).
    """
    dataset = create_dataset()
    days = all_days_since(start)

    for i, day in enumerate(days):
        dataset.to_excel(f'dataset_{q}_bkp.xlsx', encoding='latin-1', index=False) if i % 7 == 0 else None
        print(day)

        try:
            response, q = get_request_page(q=q, date=str(day.date()), source=source)
            page = get_page(response=response)

            for http in page:
                try:
                    http = http.get('href')
                    dataset.loc[len(dataset)] = [day, http]

                except Exception as e1:
                    err_name = type(e1).__name__
                    print(f'Não achou o link {http}: {err_name}')
                    continue

        except Exception as e2:
            err_name = type(e2).__name__
            print(f'Requisition error for URL {http}: {err_name}')
    
    dataset['endereco'] = "https:" + dataset['endereco']
    dataset.to_excel(f'dataset_{q}.xlsx')
    
    return dataset, q


def get_http_text(dataset: pd.DataFrame, q: str) -> pd.DataFrame:
    """
    Extrai o texto completo das notícias a partir dos URLs presentes no dataset e os armazena no dataset atualizado.

    Parâmetros:
    - dataset (pd.DataFrame): Dataset contendo as URLs das notícias.
    - q (str): Termo de pesquisa.

    Retorna:
    - pd.DataFrame: Dataset atualizado contendo o texto completo de cada notícia.
    """
    textos_completos = []

    https = dataset['endereco'].to_list()
    restantes = 0
    
    for i, http in enumerate(https):
        print(f"Restam {len(https)-restantes} consultas")
        sessao = requests.Session()
    
        try:
            url = f'{http}'
            req = sessao.get(url, timeout=15.0)
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
    
        # Em caso de redirecionamento de link
        except AttributeError as e2:
            try:
                script_redirecionamento = soup.find('script', string=re.compile(r'window\.location\.replace\("(.+)"\)'))
                match = re.search(r'window\.location\.replace\("(.+)"\)', str(script_redirecionamento))
                url_redirecionada = match.group(1)

                print(f'{e2} redirecionado para {url_redirecionada}')
            
                req = requests.get(url_redirecionada, timeout=300.0)
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
                restantes += 1
                sessao.close()
                continue

        except Exception as e:
            texto_completo = "N/D"
            textos_completos.append(texto_completo)
            pd.DataFrame(textos_completos).to_excel(f'textos_completos_{q}.xlsx', encoding='latin-1', index=False) if i % 100 == 0 else None
            print(f"Erro ao obter URL redirecionada: {e}")
            time.sleep(600)
            restantes += 1
            sessao.close()
            continue

    dataset['Texto'] = textos_completos
    dataset.to_excel(f'textos_completos_{q}.xlsx', index=False)

    return dataset
