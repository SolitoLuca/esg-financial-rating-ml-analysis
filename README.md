# esg-financial-rating-ml-analysis
Progetto redatto per la tesi di laurea triennale 2024. Analisi dell'impatto degli indicatori ESG sui rating finanziari delle aziende dello S&amp;P 500 tramite modelli di Machine Learning (Regressione Logistica, Random Forest, GBM) sviluppati in R.

# Gli indicatori ESG influiscono sulla valutazione di una azienda? 🌍📈
**Un'analisi Machine Learning sui componenti dello S&P 500**

Questo repository contiene il progetto di ricerca e il codice relativo alla mia tesi triennale. L'obiettivo è determinare se, e in che misura, i parametri Environmental, Social e Governance (ESG) condizionino i rating finanziari (Investment vs Speculative Grade) delle principali aziende americane.

## 📌 Panoramica del Progetto
Il progetto analizza un dataset di 503 aziende dello S&P 500, integrando dati finanziari e rating ESG provenienti da **Factset**, **FTSE Russell** e **Truevalue Labs**. 

### Domanda di Ricerca
> I parametri ESG sono statisticamente significativi nella determinazione della valutazione creditizia di un'impresa o rimangono marginali rispetto ai classici indicatori di bilancio?

---

## 🛠️ Tech Stack & Metodologia
* **Linguaggio:** R
* **Data Cleaning & Imputation:** Winsorizzazione per gli outlier, `missForest` (Random Forest) per la gestione dei valori mancanti.
* **Modelli Parametrici:**
  * Regressione Logistica (GLM)
  * Selezione Stepwise (Forward/Backward)
  * Regolarizzazione (Ridge, Lasso, Elastic Net)
* **Modelli Algoritmici:**
  * Random Forest
  * Gradient Boosting Machine (GBM) - *Risultato come miglior modello predittivo.*

---

## 📊 Risultati Chiave
* **Performance:** Il modello **Gradient Boosting Machine (GBM)** ha ottenuto i risultati migliori con un $AUROC = 0.727$ e un'accuratezza del **65.9%** sul test set.
* **Impact:** Gli indici ESG mostrano un'influenza positiva ma non predominante.
* **Variabile Leader:** Il rapporto $E/P$ (Earnings/Price) è risultato il predittore più significativo in tutti i modelli testati, riflettendo una forte correlazione negativa con il rischio di downgrade.

---

## 📁 Struttura del Repository
* `/code`: Script R per la pulizia dei dati, l'EDA e la stima dei modelli.
* `/data`: Dataset.
* `/docs`: tesi in PDF.

---
**Contatti:** [Luca Solito](https://www.linkedin.com/in/luca-solito-2a561b1b5)  
**Università:** Università degli Studi di Firenze - Scuola di Economia e Management
