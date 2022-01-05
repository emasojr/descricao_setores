# Carregando pacotes
library(BatchGetSymbols)
library(tidyverse)
library(reshape2)
library(PerformanceAnalytics)
library(psych)

# Separando código de Ações por setor econômico
# Bens Industriais
bi = c('AERI3.SA','ALPK3.SA','ARML3.SA','ATMP3.SA','AZEV3.SA','AZEV4.SA','AZUL4.SA','BDLL3.SA','BDLL4.SA','CCRO3.SA','DTCY3.SA','EALT3.SA','EALT4.SA','ECOR3.SA','EMBR3.SA','EPAR3.SA','ETER3.SA','FRAS3.SA','FRIO3.SA','GGPS3.SA','GOLL4.SA','HAGA3.SA','HAGA4.SA','HBSA3.SA','INEP3.SA','INEP4.SA','JSLG3.SA','KEPL3.SA','LOGN3.SA','LUXM4.SA','MILS3.SA','MRSA3B.SA','MRSA6B.SA','MTSA4.SA','MWET4.SA','POMO3.SA','POMO4.SA','PORT3.SA','PRNR3.SA','PTBL3.SA','RAIL3.SA','RAPT3.SA','RAPT4.SA','RCSL3.SA','RCSL4.SA','ROMI3.SA','RSUL4.SA','SEQL3.SA','SHUL4.SA','STBP3.SA','TASA3.SA','TASA4.SA','TCNO3.SA','TCNO4.SA','TGMA3.SA','TPIS3.SA','TUPY3.SA','VLID3.SA','WEGE3.SA','WLMM4.SA')
# Comunicações
co = c('BRIT3.SA','DESK3.SA','ELMD3.SA','FIQE3.SA','OIBR3.SA','OIBR4.SA','TELB3.SA','TELB4.SA','TIMS3.SA','VIVT3.SA')
# Consumo Cíclico
cc = c('AHEB3.SA','AHEB5.SA','ALLD3.SA','ALPA3.SA','ALPA4.SA','AMAR1.SA','AMAR3.SA','AMER3.SA','ANIM3.SA','ARZZ3.SA','AVLL3.SA','BAHI3.SA','BKBR3.SA','BMKS3.SA','CAMB3.SA','CEAB3.SA','CEDO3.SA','CEDO4.SA','CGRA3.SA','CGRA4.SA','COGN3.SA','CRDE3.SA','CSED3.SA','CTKA3.SA','CTKA4.SA','CTNM3.SA','CTNM4.SA','CTSA3.SA','CTSA4.SA','CURY3.SA','CVCB3.SA','CYRE3.SA','DIRR3.SA','DOHL4.SA','DOTZ3.SA','ESPA3.SA','EVEN3.SA','EZTC3.SA','GFSA3.SA','GRND3.SA','GUAR3.SA','HBOR3.SA','HETA4.SA','HOOT4.SA','JFEN3.SA','JHSF3.SA','LAME3.SA','LAME4.SA','LAVV3.SA','LCAM3.SA','LEVE3.SA','LJQQ3.SA','LLIS3.SA','LREN3.SA','MDNE3.SA','MEAL3.SA','MELK3.SA','MGLU3.SA','MNDL3.SA','MOVI3.SA','MRVE3.SA','MTRE3.SA','MYPK3.SA','PDGR3.SA','PETZ3.SA','PLAS3.SA','PLPL3.SA','PTNT3.SA','PTNT4.SA','RDNI3.SA','RENT3.SA','RSID3.SA','SEER3.SA','SGPS3.SA','SHOW3.SA','SLED3.SA','SLED4.SA','SMFT3.SA','SOMA3.SA','TCSA3.SA','TECN3.SA','TEKA4.SA','TEND3.SA','TFCO4.SA','TRIS3.SA','TXRX4.SA','UCAS3.SA','VAMO3.SA','VIIA3.SA','VIVA3.SA','VIVR3.SA','VULC3.SA','WHRL3.SA','WHRL4.SA','YDUQ3.SA')
# Consumo não Cíclico
cn = c('ABEV3.SA','AGRO3.SA','AGXY3.SA','ASAI3.SA','BAUH4.SA','BEEF3.SA','BOBR4.SA','BRFS3.SA','CAML3.SA','CRFB3.SA','FRTA3.SA','GMAT3.SA','JALL3.SA','JBSS3.SA','JOPA3.SA','LAND3.SA','MDIA3.SA','MNPR3.SA','MRFG3.SA','NTCO3.SA','PCAR3.SA','SLCE3.SA','SMTO3.SA','SOJA3.SA','TTEN3.SA')
# Financeiro
fi = c('ABCB4.SA','ALSO3.SA','APER3.SA','B3SA3.SA','BAZA3.SA','BBAS3.SA','BBDC3.SA','BBDC4.SA','BBRK3.SA','BBSE3.SA','BEES3.SA','BEES4.SA','BGIP4.SA','BIDI11.SA','BIDI11.SA','BIDI3.SA','BIDI4.SA','BMEB3.SA','BMEB4.SA','BMGB4.SA','BMIN4.SA','BNBR3.SA','BOAS3.SA','BPAC11.SA','BPAC3.SA','BPAC5.SA','BPAN4.SA','BRBI11.SA','BRGE12.SA','BRGE3.SA','BRIV3.SA','BRIV4.SA','BRML3.SA','BRPR3.SA','BRSR3.SA','BRSR5.SA','BRSR6.SA','BSLI3.SA','BSLI4.SA','CARD3.SA','CIEL3.SA','CLSA3.SA','CRIV3.SA','CRIV4.SA','CSAB4.SA','CXSE3.SA','G2DI33.SA','GETT11.SA','GETT3.SA','GETT4.SA','GPIV33.SA','HBRE3.SA','IGBR3.SA','IGTI11.SA','IGTI11.SA','IGTI3.SA','IRBR3.SA','ITSA3.SA','ITSA4.SA','ITUB3.SA','ITUB4.SA','LOGG3.SA','LPSB3.SA','MOAR3.SA','MODL11.SA','MODL11.SA','MODL3.SA','MODL4.SA','MULT3.SA','PEAB3.SA','PINE4.SA','PPLA11.SA','PSSA3.SA','RPAD3.SA','RPAD5.SA','RPAD6.SA','SANB11.SA','SANB3.SA','SANB4.SA','SCAR3.SA','SIMH3.SA','STOC31.SA','SULA11.SA','SULA3.SA','SULA4.SA','SYNE3.SA','WIZS3.SA','XPBR31.SA')
# Materiais Básicos
mb = c('BRAP3.SA','BRAP4.SA','BRKM3.SA','BRKM5.SA','CBAV3.SA','CMIN3.SA','CRPG3.SA','CRPG5.SA','CRPG6.SA','CSNA3.SA','DEXP3.SA','DEXP4.SA','DXCO3.SA','EUCA3.SA','EUCA4.SA','FESA3.SA','FESA4.SA','FHER3.SA','GGBR3.SA','GGBR4.SA','GOAU3.SA','GOAU4.SA','KLBN11.SA','KLBN3.SA','KLBN4.SA','MGEL4.SA','MTIG4.SA','PMAM3.SA','RANI3.SA','SNSY5.SA','SUZB3.SA','UNIP3.SA','UNIP5.SA','UNIP6.SA','USIM3.SA','USIM5.SA','VALE3.SA','VITT3.SA')
# Outros
ou = c('ATOM3.SA','BLUT3.SA','BLUT4.SA','MAPT4.SA')
# Petróleo, Gás e Biocombustíveis
pg = c('CSAN3.SA','DMMO3.SA','ENAT3.SA','LUPA3.SA','OPCT3.SA','OSXB3.SA','PETR3.SA','PETR4.SA','PRIO3.SA','RECV3.SA','RPMG3.SA','RRRP3.SA','UGPA3.SA','VBBR3.SA')
# Saúde
su = c('AALR3.SA','BALM3.SA','BALM4.SA','BIOM1.SA','BIOM3.SA','BLAU3.SA','DASA3.SA','DMVF3.SA','FLRY3.SA','GNDI3.SA','HAPV3.SA','HYPE3.SA','KRSA3.SA','MATD3.SA','ODPV3.SA','OFSA3.SA','ONCO3.SA','ONCO3.SA','PARD3.SA','PFRM3.SA','PGMN3.SA','PNVL3.SA','QUAL3.SA','RADL3.SA','RDOR3.SA','VVEO3.SA')
# Tecnologia da Informação
ti = c('BMOB3.SA','CASH3.SA','ENJU3.SA','IFCM3.SA','INTB3.SA','LVTC3.SA','LWSA3.SA','MBLY3.SA','MLAS3.SA','MOSI3.SA','NGRD3.SA','NINJ3.SA','PDTC3.SA','POSI3.SA','SQIA3.SA','TOTS3.SA','TRAD3.SA','WEST3.SA')
# Utilidade Pública
up = c('AESB3.SA','AFLT3.SA','ALUP11.SA','ALUP3.SA','ALUP4.SA','AMBP3.SA','CBEE3.SA','CEBR3.SA','CEBR5.SA','CEBR6.SA','CEEB3.SA','CEED3.SA','CEGR3.SA','CEPE5.SA','CEPE6.SA','CESP3.SA','CESP6.SA','CGAS3.SA','CGAS5.SA','CLSC3.SA','CLSC4.SA','CMIG3.SA','CMIG4.SA','COCE3.SA','COCE5.SA','CPFE3.SA','CPLE11.SA','CPLE3.SA','CPLE5.SA','CPLE6.SA','CSMG3.SA','CSRN3.SA','EGIE3.SA','EKTR4.SA','ELET3.SA','ELET5.SA','ELET6.SA','EMAE4.SA','ENBR3.SA','ENEV3.SA','ENGI11.SA','ENGI3.SA','ENGI4.SA','ENMT3.SA','ENMT4.SA','EQMA3B.SA','EQPA3.SA','EQTL3.SA','GEPA4.SA','LIGT3.SA','LIPR3.SA','NEOE3.SA','ORVR3.SA','POWE3.SA','REDE3.SA','RNEW11.SA','RNEW3.SA','RNEW4.SA','SAPR11.SA','SAPR3.SA','SAPR4.SA','SBSP3.SA','TAEE11.SA','TAEE3.SA','TAEE4.SA','TRPL3.SA','TRPL4.SA')
# todos
geral = c(bi,co,cc,cn,fi,mb,ou,pg,su,ti,up)
geral = unique(geral)

# Criando Lista com vetores
setores = list(bens_industriais = bi,
               comunicacao = co,
               consumo_ciclico = cc,
               consumo_n_ciclico = cn,
               financa = fi,
               materiais_basicos = mb,
               outros = ou,
               petroleo_gas = pg,
               saude = su,
               tecnologia = ti,
               utilidade_pub = up,
               gg = geral)
nomes = c('Bens Industriais','Comunicações','Consumo Cíclico','Consumo não Cíclico',
          'Finanças','Materiais Básicos','Outros','Petróleo, Gás e Biocombustíveis',
          'Saúde','Tecnologia da Informação','Utilidade Pública', 'Geral')

# Construindo o índice dos setores
inicio = as.Date('01/01/2010',format = '%d/%m/%Y')
fim = Sys.Date()-1
for (j in 1:length(nomes)) {
  # Coletando Dados
  dados = BatchGetSymbols(tickers = setores[[j]],
                          first.date = inicio,
                          last.date = fim,
                          thresh.bad.data = 1,
                          bench.ticker = '^BVSP')
  
  # Ajustando data frame de preços
  preco = data.frame(dados$df.tickers$ref.date, dados$df.tickers$ticker, dados$df.tickers$price.close)
  colnames(preco) = c('data','ticker','preco')
  preco = dcast(preco, data ~ ticker, value.var = 'preco')
  for (i in 2:ncol(preco)) {
    preco[,i] = (preco[,i]/preco[1,i])*100
  }
  
  # Ajustando volume negociado
  volume = data.frame(dados$df.tickers$ref.date, dados$df.tickers$ticker, dados$df.tickers$volume)
  colnames(volume) = c('data','ticker','vol')
  volume = dcast(volume, data ~ ticker, value.var = 'vol')
  volume = volume %>% filter(data >= '2021-01-01')
  
  # Calculando os pesos
  peso = as.matrix(colSums(volume[,2:ncol(volume)])/sum(colSums(volume[,2:ncol(volume)])))
  
  # Calculando comportamento do setor através de um Índice de Paasche
  if (j == 1){
    # Se for a primeira volta, cria o data frame
    setor = data.frame(preco$data, as.matrix(preco[,2:ncol(preco)]) %*% peso)
    colnames(setor) = c('data',nomes[j])
  } else{
    # Se for maior que a primeira volta, uni os data frames
    price = as.matrix(preco[,2:ncol(preco)]) %*% peso
    setor = data.frame(setor, price)
    colnames(setor) = c('data', nomes[1:j])
  }
}

# Gráficos do movimento dos setores
melt(setor, id.vars = 'data') %>% ggplot() +
  aes(x = data, y = value, colour = variable) +
  geom_line(size = 1L) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Data",
    y = "Índice",
    title = "Desempenho dos setores da B3 no mercado de Ações",
    subtitle = "Índice das principais ações no período jan/2010 - dez/2021",
    caption = "Fonte: Yahoo Finance | Elaboração: Evânio Marques"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(vars(variable), scales = "free")

# Criando retornos
return = xts(setor[,2:ncol(setor)], order.by = setor$data)
retorno = data.frame(setor$data, CalculateReturns(return, method = 'log'))
retorno = retorno[rowSums(is.na(retorno))==0,]
return = CalculateReturns(return, method = 'log')
return = return[rowSums(is.na(return))==0,]

# Correlação de Setores
corPlot(retorno[,2:ncol(retorno)],
        scale = FALSE,
        main = 'Correlação entre setores',
        upper = FALSE)

# Betas
tabela = data.frame()
for (i in 1:ncol(return)) {
  tabela[i,1] = round(CAPM.beta(Ra = return[,i],
            Rb = return$Geral), digits = 4)
}
rownames(tabela) = nomes
colnames(tabela) = 'CAPM - Betas'

# Value at Risk
for (i in 1:ncol(return)) {
  tabela[i,2] = round(VaR(R = return[,i], p = 0.95,
                    method = 'historical')*100, digits = 2)
}
colnames(tabela)[2] = 'Value at Risk'

# Retorno Anualizado
for (i in 1:ncol(return)) {
  tabela[i,3] = round(Return.annualized (R = return[,i], scale = 252)*100, digits = 2)
}
colnames(tabela)[3] = 'Retorno Anualizado'

# Montando tabela
as.matrix(tabela)