#lang racket
(require examples)

;; MEMBRO:
;; EDUARDO FELIPPE BOREGIO RUFINO RA:129379
;; OBS: CORRIJA COM CARRINHO PROFESSOR ;)..... 

;; EXERCICIO 1
;;1) Projete uma função que determine a velocidade que deve ser considerada a partir da velocidade medida por um
;;   radar.
;;
;; ANALISE
;; Calcula a velocidade-medida atraves das regras que foram dadas pelo codigo de transito,
;; caso a velocidade seja menor ou igual a 107 deve diminuir 7 deste valor,
;; caso seja maior que 107 voce deve diminuir 7% deste valor.
;;
;; TIPOS DE DADOS
;; Velocidade = Recebe a velocidade medida pelo radar.
;;
;; ESPECIFICAÇÃO
;; Calcula em qual parametro a velocidade medida deve entrar
;; Caso a velocidade-medida seja menor ou igual a 107km/h ele ira retirar 7km/h
;; da velocidade-medida, caso contrario deve diminuir 7% da velocidade-medida
;;
;; EXEMPLO
(examples
 (check-equal?(velocidade 107.0)100.0)
 (check-equal?(velocidade 93.0) 86.0)
 (check-equal?(velocidade 108.0)100.44)
 (check-equal?(velocidade 125.0)116.25)
 (check-equal?(velocidade 140.0)130.2)
 (check-equal?(velocidade 50.0) 43.0)
 (check-equal?(velocidade 200.0)186.0))

;; IMPLEMENTAÇÃO

(define(velocidade velocidade-medida)
  (cond
    [(<= velocidade-medida 107)(- velocidade-medida 7)]
    [(> velocidade-medida 107)(- velocidade-medida(* velocidade-medida 0.07))]))


;; EXERCICIO 2
;; 2) Projete uma função que determine a partir da velocidade medida por um radar e do limite de velocidade da via,
;; qual o tipo de infração por excesso de velocidade, se alguma, foi cometida pelo motorista do veículo.
;;
;; ANALISE
;; Com base no enunciado do exercicio iremos ultilizar o parametro do exercicio anterior para estar calculando qual
;; foi a gravidade da multa levada pelo motorista, este valor sera calculado junto com o limite da via.
;;
;; TIPOS DE DADODS
;;
;; velocidade-medida = RECEBE UM VALOR NUMERICO - Foi usada no exercicio anterior sera ultilizada neste tambem.w
;; limite-velocidade = RECEBE UM VALOR NUMERICO - Ira indicar qual é o limite da via em que esta circulando.
;; infracao-de-velocidade = Recebe todas essas funcoes acima.
;; Temos os niveis de infracoes (Leve, Media, Grave, Gravissima), estes niveis sera informados para o motorista.
;;
;; ESPECIFICAÇÕES 
;; Sera ultilizado o parametro de velocidade-medida e limite-velocidade para estar calculando qual foi o nivel
;; da gravidade da infraçao recebida pelo motorista (Leve, Media, Grave, Gravissima).
;;
;; EXEMPLO
(examples
 (check-equal?(infracao-de-velocidade 60  107)"Nivel Gravissima")
 (check-equal?(infracao-de-velocidade 80   93)"Nivel Media")
 (check-equal?(infracao-de-velocidade 100 108)"Nivel Media")
 (check-equal?(infracao-de-velocidade 80  125)"Nivel Gravissima")
 (check-equal?(infracao-de-velocidade 110 140)"Nivel Grave")
 (check-equal?(infracao-de-velocidade 40  50 )"Nivel Grave")
 (check-equal?(infracao-de-velocidade 70  200)"Nivel Gravissima"))

;; IMPLEMENTAÇÃO

(define(infracao-de-velocidade limite-velocidade velocidade-medida)
  (cond
    [(<=(/(- velocidade-medida limite-velocidade)limite-velocidade)0.2)"Nivel Media"]
    [(<=(/(- velocidade-medida limite-velocidade)limite-velocidade)0.5)"Nivel Grave"]
    [(>(/(- velocidade-medida limite-velocidade)limite-velocidade)0.5)"Nivel Gravissima"]))


;; EXERCICOS 3
;; 3) Projete uma função que determine se a CNH de um motorista deve ser suspensa por ultrapassar o limite depontos.
;;
;; ANALISE
;; Como foi descrito no enunciado deve-se calcular atraves da quantidade de infraçoes recebida pelo motorista se a sua
;; carteira de motorista esta "Suspensa" ou "Não suspensa". 
;; 
;; TIPOS DE DADODS
;; pontos-sofridos = Delimita a quantidade de pontos que o motorista pode atingir (20,30 ou 40 pontos),
;; situacao-CNH = Armazena qual seria a situaçao que a carteira de motorista esta ("Suspensa" ou "Não suspensa"),
;; multa = Especifica os tipos de infraçoes leve, media, grave, gravissima,
;; soma-pontos = Soma a quantidade de pontos sofridos pelo motorista, sendo que cada infraçao a seu peso sendo a 
;; leve = 3 pontos, media = 4 pontos, grave = 5 pontos, gravissima = 7 pontos.
;; 
;; ESPECIFICAÇÕES 
;; Calcula a qualtidade de ponto sofridos pelo motorista, caso o motorista tenha uma multa grave ou gravissima os limites de
;; pontos serai calculado de uma forma diferente, motoristas sem multas grave ou gravissima limite de 40 pontos, motorista com
;; uma multa grave ou gravissima limite de 30 pontos, ja para o restante mais que uma multa grave ou gravissima recebe o limite
;; de  20 pontos.
;;
;; EXEMPLO


;;CRITICAS
;;- Não tem uma descrição do propósito da estrutura multa.
;;- Não tem assinatura
;;- cond usado sem consequente
;;- Não passa nos testes
;;

(examples
 (check-equal?(situacao-CNH(multa 0 2 5 6))"Esta suspensa")
 (check-equal?(situacao-CNH(multa 1 2 8 1))"Esta suspensa")
 (check-equal?(situacao-CNH(multa 2 4 8 3))"Esta suspensa")
 (check-equal?(situacao-CNH(multa 0 0 0 0))"Não esta suspensa")
 (check-equal?(situacao-CNH(multa 1 1 0 0))"Não esta suspensa")
 (check-equal?(situacao-CNH(multa 6 4 4 1))"Esta suspensa"))

;; IMPLEMENTAÇÃO

(struct multa (leve media grave gravissima))
(define(situacao-CNH pontos-sofridos)
  (define soma-pontos
  (+(* (multa-leve pontos-sofridos)3)(*(multa-media pontos-sofridos)4)
    (*(multa-grave pontos-sofridos)5)(*(multa-gravissima pontos-sofridos)7)))  
(cond
  [(or (and(=(multa-grave pontos-sofridos)0)(=(multa-gravissima pontos-sofridos)0))
     (if(<= soma-pontos 40)"Esta suspensa" "Não esta suspensa"))]
  [(or (and(=(multa-grave pontos-sofridos)1)(=(multa-gravissima pontos-sofridos)1))
     (and(=(multa-media pontos-sofridos)0)(=(multa-leve pontos-sofridos)0))
     (if (<= soma-pontos 30) "Esta suspensa" "Não esta suspensa"))]
  [else
     (if (<= soma-pontos 20) "Esta suspensa" "Não esta suspensa")]))
