

# Carregar pacotes
library(tidyverse)
library(dplyr)
library(stringr)
library(tidytext)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(wesanderson)
library(plotly)
library(DT)

# Criar um vetor com as palavras/frases fornecidas
daddos_alegria <- c(
  "Falar com Deus, jogar bola, anda de bike com os amigos",
  "Brincar, comer, estudar e andar de ônibus",
  "Ficar feliz, brincar",
  "É divertir",
  "Brincar com irmãos, assistir televisão",
  "Flor, eu gosto de flor",
  "Comida",
  "Ver o sol, comer caju, come bolo",
  "Tomar banho na praia",
  "Ter um cachorrinho",
  "Ir para praia, ir pra aula, ir pra Santarém, ir no centro",
  "É quando fico alegre",
  "Quando eu lavo louça",
  "É brincar",
  "Assistir televisão",
  "Alegria é quando estamos alegre, quando dão as coisa pra gente fica alegre",
  "Não sei",
  "Quando a gente tá feliz",
  "Alegria é estudar, fico feliz porque a mamãe compra tudo pra mim comer quando estou com fome, é ficar alegre",
  "Alegria é quando estamos estudando",
  "Brincar e se divertir",
  "Brincar",
  "Ficar mamãe, gosto da mamãe, vovô",
  "Quando a gente ficar estudando",
  "Alegria é brincar com minhas irmãs e minha prima Andressa",
  "Respeitar, obedecer a mae, fazer tudo o que ela mandar e eu ajudar ela",
  "Diversão, gosto de brincar, pescar, joga bola e ir na praia",
  "Dormir la em casa",
  "Ter família",
  "É brincar e correr",
  "Quando sinto felicidade e vejo alguém que amo",
  "É meus irmãos. Eles são especiais para mim",
  "Quando ganho alguma coisa, e quando a mamãe me leva pro parque",
  "Jesus, ir no rio pegar peixinho, concha e sapinho",
  "Morar para santarém",
  "Ir pra casa. Ir com a mamãe e papai. Quando vai pra casa fico alegre",
  "Papai, a vovó",
  "QUANDO A GENTE FICA ALEGRE COM UMA PESSOA",
  "É brincar",
  "Amor e carinho",
  "Brincar, quando tenho cachorrinho, quando to tomando banho na praia",
  "Só quando eu brinco com meus coleguinhas",
  "É a gente brincar e se divertir e passear",
  "A mamãe, porque ela é bonita, o Gabriel me deixa alegre, porque ele não briga. Eu lavo prato.",
  "Brincar com as pessoas de boneca. Brincar com os irmãos. Cuidar do bebê. Dar banho e coloca fralda com a ajuda da irmã",
  "Quando a gente ficar estudando",
  "É animado, com presente, papai quando vem, traz algum presente ora mim, muito animado.",
  "Sorrir (sorrio e gargalhou)",
  "Dormir la em casa",
  "Gostar do pai, irmão e da mãe",
  "Fazer trabalho, estudar, brincar",
  "É FELIZ"
)


dados_tristeza <- c(
  "Não jogar bola e não andar de bike",
  "Triste, chorar, falarem mal de mim e me empurrar",
  "Quando fico brabo, não compartilhar o brinquedo com o amigo",
  "Chorar",
  "Quando o Heitor bate em mim (irmão), mas o Hercules não (outro irmão), e quando brigam por causa de brinquedo",
  "A minha vó morreu",
  "Chuva",
  "Gosto de garrafa verde e ninguém deixa, triste",
  "Não ir na praia",
  "Quando minha mãe vai embora",
  "Não sei",
  "Quando eu fico triste quando acontece alguma coisa e fico triste",
  "Quando me jogam ovo no meu aniversário",
  "É o sol, porque deixa muito quente",
  "Feliz",
  "Quando fica chorando",
  "Quando meu amigo me chuta ai fico triste",
  "Quando a gente tá triste, umas vezes a gente chora, outras não chora",
  "Não sei como é",
  "Fico chorando, triste",
  "Fico mal e fico doente",
  "Saudade da avó",
  "Tristeza, saudade",
  "Quando alguém fica brabo",
  "É ver minhas irmãs brigando e parando de brincar comigo",
  "Quando minha mae bate na minha irmã Yara, fazer vergonha e não dizer desculpa",
  "Quando a mamãe bate e a gente chora",
  "Não sei",
  "Não ser obediente com a mãe",
  "Partir o coração",
  "Quando alguém morre",
  "É quando meus pais me batem",
  "Quando o papai vai embora pra santarém",
  "Quando a concha fura meu pé e quando o lago é muito fundo",
  "Não sei fiquei triste porque conhecia ela (avó)",
  "Quando venho pra escola e o papai vai embora, eu choro. Quando papai deixa na escola e vai embora",
  "Papai, porque ele só briga",
  "QUANDO FICA TRISTE, QUANDO UM APESSOA MENTE PRA MIM EU FICO MUITO TRISTE",
  "Quando falam mal de mim",
  "Chorar",
  "Quando minha mae me bate, quando não tenho brinquedo",
  "Não tenho tristeza",
  "É a gente ficar sozinho e não querer barulho",
  "É não beliscar, porque quando o coleguinha senta perto e não é pra beliscar. Dor de cabeça. Dor de garganta. Dodói.",
  "A mãe ralha. Mãe bate. Mãe castiga. Na escola quando o colega puxa o cabelo",
  "Quando alguém fica brabo",
  "Quando chora, quando alguém bate, corro pra minha mãe.",
  "Chorar",
  "Não sei",
  "A mãe rabiscar a atividade. Não fazer birra e nem chamar palavrão",
  "Quando eu caio, quando fico brabo, quando estou doente",
  "É TRISTE, CHORA"
)


# Armazenar no objeto chamado corpus1
corpus_alegria <- Corpus(VectorSource(dados))
corpus_tristeza <- Corpus(VectorSource(dados_tristeza))

# Pré-processamento de texto: converter para minúsculas, remover pontuação, etc.
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, removePunctuation)
corpus1 <- tm_map(corpus1, removeNumbers)
corpus1 <- tm_map(corpus1, removeWords, stopwords("portuguese"))
corpus1 <- tm_map(corpus1, stripWhitespace)


corpus_tristeza <- tm_map(corpus_tristeza, content_transformer(tolower))  # Converter para minúsculas
corpus_tristeza <- tm_map(corpus_tristeza, removePunctuation)             # Remover pontuação
corpus_tristeza <- tm_map(corpus_tristeza, removeNumbers)                 # Remover números
corpus_tristeza <- tm_map(corpus_tristeza, removeWords, stopwords("portuguese"))  # Remover palavras comuns
corpus_tristeza <- tm_map(corpus_tristeza, stripWhitespace)


# Criar uma matriz de termos
tdm <- TermDocumentMatrix(corpus1)
tdm_tristeza <- TermDocumentMatrix(corpus_tristeza)

# Converter a matriz de termos em uma matriz de dados
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)


m_tristeza <- as.matrix(tdm_tristeza)
word_freqs_tristeza <- sort(rowSums(m_tristeza), decreasing = TRUE)


# Criar a nuvem de palavras Alegria
wordcloud(names(word_freqs), 
          word_freqs, 
          min.freq = 1,                        # Frequência mínima de palhttp://127.0.0.1:15825/graphics/d5b9c343-c6e6-422d-9186-87dfde8fefb6.pngavras a serem exibidas
          max.words = 200,                     # Número máximo de palavras a serem exibidas
          random.order = FALSE,
          random.color = TRUE,
          scale=c(6,0.5), 
          colors=brewer.pal(8, "Dark2")
          )



wordcloud(names(word_freqs_tristeza), 
          word_freqs_tristeza, 
          min.freq = 1,                        # Frequência mínima de palhttp://127.0.0.1:15825/graphics/d5b9c343-c6e6-422d-9186-87dfde8fefb6.pngavras a serem exibidas
          max.words = 200,                     # Número máximo de palavras a serem exibidas
          random.order = FALSE,
          random.color = TRUE,
          scale=c(6,0.5), 
          colors=brewer.pal(8, "Dark2")
)


















