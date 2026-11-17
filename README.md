# 📊 Modelos eleitorais: painel e cortes transversais

Este projeto reúne três especificações usadas para analisar o voto do incumbente nas Unidades da Federação: **Lewis-Beck**, **Abramovitz** e **R. Lins**. O código mantém as fórmulas do projeto e acrescenta comparações entre modelos, erros-padrão robustos e diagnósticos inspirados nos capítulos 13 e 14 de Wooldridge.

> A base é agregada por UF e eleição. As estimativas descrevem associações entre variáveis; não devem ser interpretadas como efeitos causais sem hipóteses adicionais de identificação.

## Início rápido

Abra `Modelos_Wooldridge_Cap13_14.Rproj` no RStudio, com o R instalado. Na raiz do projeto, execute os scripts nesta ordem:

```r
source("R/00_construir_base.R")
source("R/01_estimacoes_com_conceitos_aula.R")
```

O primeiro script recria `dados/BaseDados1.csv`. O segundo estima os modelos e mostra tabelas e diagnósticos. Os scripts instalam os pacotes necessários quando eles ainda não estão disponíveis.

O script de estimação está organizado em blocos separados para Lewis-Beck, Abramovitz, R. Lins 2018 e R. Lins 2022. Ele repete as etapas em cada bloco em vez de escondê-las em funções auxiliares, para que seja mais fácil acompanhar o que cada comando faz.

## Modelos

| Modelo | Estrutura dos dados | Especificação principal |
| --- | --- | --- |
| **Lewis-Beck** | Painel de UFs por ano eleitoral | Efeitos aleatórios: `voto_inc_pct ~ Var + Inc_Aprov` |
| **Abramovitz** | Mesmo painel, com indicador institucional | Efeitos aleatórios: `voto_inc_pct ~ Var + Inc_Aprov + third_term` |
| **R. Lins** | Corte transversal estimado separadamente em 2018 e 2022 | MQO: `voto_inc_pct ~ IBF` |

`voto_inc_pct` está em pontos percentuais. O painel reúne eleições de 1994 a 2022, em ciclos de quatro anos; não é uma série anual. A observação de 2024 contém o IBF, sem resultado eleitoral, e fica fora das estimações de painel.

## O que o código acrescenta

### Lewis-Beck e Abramovitz

- Estimação comparativa por MQO agrupado, efeitos fixos (*within*), efeitos aleatórios (*random*), primeiras diferenças e *between*.
- Teste de Hausman entre efeitos fixos e aleatórios.
- Erros-padrão Arellano agrupados por UF e CR2 para o modelo aleatório ponderado.
- Diagnósticos de autocorrelação serial, dependência entre UFs e heterocedasticidade.
- Gráficos de resíduos, QQ-plots e testes complementares de normalidade.
- Valores ajustados agregados por ano e ponderados pela população.

As comparações didáticas e o Hausman são **sem pesos**, usando a mesma amostra e escala. A estimação principal de efeitos aleatórios é **ponderada pela população**, como nos scripts originais. O estimador de primeiras diferenças também é sem pesos: os pesos em nível não podem ser reaproveitados diretamente após diferenciar os dados.

### R. Lins

O modelo é estimado por MQO separadamente para 2018 e 2022. O código apresenta erros-padrão HC1 e HC3, Breusch-Pagan, inspeção dos resíduos e gráficos da relação entre IBF e voto. A média nacional das previsões é ponderada pela população.

Como cada estimação tem uma observação por UF, os testes de Hausman e de autocorrelação de painel não se aplicam a R. Lins.

### Diferenças em diferenças

O conceito é explicado no código, mas não estimado. As três especificações não definem grupo de tratamento, grupo de controle e evento de política com períodos pré e pós. Criar a interação sem esse desenho implicaria inventar uma estratégia de identificação.

## Leitura dos diagnósticos desta execução

Os scripts foram executados localmente com R 4.4.2. A base gerada contém **244 linhas**: 217 observações históricas e 27 registros de IBF para 2024. Os dois scripts terminaram sem erro.

- O Hausman não rejeitou a hipótese nula nas comparações sem pesos: Lewis-Beck, `p = 0,773`; Abramovitz, `p = 0,914`.
- Os testes apontaram dependência entre UFs nos dois modelos de painel.
- Os testes de autocorrelação discordaram entre si; o resultado deve ser lido com cautela.
- O Breusch-Pagan apontou heterocedasticidade no modelo de Abramovitz (`p = 0,002`). Para Lewis-Beck, `p = 0,074`.

Os erros-padrão agrupados por UF tratam heterocedasticidade e correlação dentro de cada estado, mas não eliminam dependência entre estados. O código reporta Driscoll-Kraay como análise de sensibilidade e ressalta que o painel tem poucos períodos eleitorais.

## Wooldridge: páginas de referência

As referências usam a **numeração impressa** no PDF fornecido.

| Conceito | Capítulo e páginas |
| --- | --- |
| Cortes transversais empilhados e interações no tempo | Cap. 13, pp. 403–409 |
| Diferenças em diferenças e análise de políticas | Cap. 13, pp. 410–412 |
| Painel de dois períodos e primeiras diferenças | Cap. 13, pp. 414–423 |
| Primeiras diferenças com mais de dois períodos | Cap. 13, pp. 424–431 |
| Transformação *within* e efeitos fixos | Cap. 14, pp. 433–440 |
| Efeitos aleatórios e erro composto | Cap. 14, pp. 441–444 |
| Comparação entre efeitos fixos e aleatórios; Hausman | Cap. 14, p. 445 |
| Outras estruturas de painel e inferência | Cap. 14, pp. 445–448 |
| Hipóteses dos modelos de efeitos fixos e aleatórios | Cap. 14, pp. 449–452 |

A normalidade dos resíduos é usada como diagnóstico complementar da aula; não há uma seção específica sobre Shapiro-Wilk ou Jarque-Bera nessas páginas dos capítulos 13 e 14.

## Arquivos

```text
Trabalho ECN 2/
├── Modelos_Wooldridge_Cap13_14.Rproj
├── R/
│   ├── 00_construir_base.R
│   └── 01_estimacoes_com_conceitos_aula.R
├── dados/
│   ├── BaseDados.csv
│   ├── BaseDados1.csv
│   ├── BaseDados1_original.csv
│   └── IBF - 2024.xlsx
└── README.md
```

- `BaseDados.csv`: insumo histórico já organizado com voto, crescimento, aprovação e IBF.
- `IBF - 2024.xlsx`: planilha do IBGE com valores estaduais do Índice Bolsa Família.
- `BaseDados1.csv`: base analítica limpa recriada pelo script.
- `BaseDados1_original.csv`: cópia preservada da versão recebida no projeto.

O código de construção reproduz a etapa registrada no histórico do projeto: acrescenta o IBF de 2024 à base histórica. A pasta original não continha os scripts e microdados primários necessários para reconstruir `BaseDados.csv` desde as fontes de voto, PIB e aprovação.
