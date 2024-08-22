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
from nltk.corpus import stopwords
from nltk.stem import SnowballStemmer
from nltk.tokenize import word_tokenize
from bs4 import BeautifulSoup
import WebScraping
import Tratamento
import SentimentAnalysis
import G1

def main():
    G1.g1_agronegocio()
    G1.g1_industria()
    G1.g1_mercado_financeiro()
    G1.g1_mercado_trabalho()
    G1.g1_servicos()

if __name__ == '__main__':
    main()