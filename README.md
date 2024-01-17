
#### Introdução

Este repositório foi criado com o intuito de manipular e visualizar os
dados do *CME FedWatch Tool*, ferramenta com propósito de mensurar as
probabilidades de mudança na taxa básica de juros americana (Fed Funds
Rate) a partir de contratos futuros (realizados na mesma taxa).

Infelizmente, os links para download das planilhas variam a cada dia e
os respectivos códigos geradores **não** estão disponíveis na estrutura
em `html` da página. O que isso significa na prática? Bom, nos mostra
que o *CME Group* não é muito amigável com quem deseja automatizar
processos com seus dados (ainda que eles estejam disponíveis). A maneira
mais eficaz de fazer o `webscraping` seria através de um processo de
automação que efetivamente entrasse na página e baixasse os dados,
clicando em cada ponto necessário, como se fosse realmente um movimento
humano. Por ter um baixo custo em termos de tempo, optei por apenas
copiar e colar o link do dia em questão.
