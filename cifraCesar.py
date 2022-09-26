# -----------------------------------------------------------------------------------------------------
# Autor: Allan Jose Amaral Ribeiro
# Programa: Cifra de Cesar
# Objetivo : Criptografar mensagens. 
# ------------------------------------------------------------------------------------------------------

# importacoes da parte responsável pelo analisador lexico e pelo analisador sintatico respectivamente
import ply.lex as lex
import ply.yacc as yacc

#--------------------------------------O alfabeto considerado-----------------------------------------------
listaAlfabeto = [
                      ("A",0),("B",1),("C",2),("D",3),("E",4),("F",5),("G",6),("H",7),("I",8),
                      ("J",9),("K",10),("L",11),("M",12),("N",13),("O",14),("P",15),("Q",16),("R",17),
                      ("S",18),("T",19),("U",20),("V",21),("X",22),("Z",23),("W",24),("Y",25)
                    ] 

# Tamanho do alfabeto
N = len(listaAlfabeto)
#-------------------------------------------------------------------------------------------------------------

# funcao que converter letra em numero e numero em letra
def transformacaoAlfanumerico(caracter, verificarLetra):
    if(verificarLetra):
        for dupla in listaAlfabeto:
            if(dupla[0] == caracter):
                return dupla[1]   
    else:
        if(-1 == caracter):
            return " "
        else:
            for dupla in listaAlfabeto:
                if(dupla[1] == caracter):
                    return dupla[0]

#------------------------------------------Analisador Lexico------------------------------------------------------

# Lista de token considerado
tokens = (
   'letra',
    'mod',
    'ch',
    'div',
    'soma',
    'vazio'
)
# expressao regular que corresponde ao token
t_ch = r'[0-9]+'  # expressao regular para chave neste caso a chave so aceita numeros
t_div = r'divisor' 
t_letra = r'[A-Z]' # expressao regular para letra neste caso so foi considerado letras maiusculas
t_vazio = r'[\s]'  # expressao regular para espacos entre frases
t_mod = r'\%'      # expressao regular para % (representa o resto da divisao em python de numero)
t_soma = r'\+'     # expressao regular para soma

# Verificador de error de caracter
def t_error(t):
    print("caracter incorreto '%s'" % t.value[0])
    print("Só aceita letras maiusculas e espacos\n ")
    t.lexer.skip(1)

# Lexico
lexer = lex.lex()

#------------------------------------------------------------------------------------------------------------

#----------------------------------Analisador sintatica e Analisador sematico-------------------------------------------------
def p_expression1(p): # primeira producao
    ''' S : S K '''
    p[2] = transformacaoAlfanumerico(int(p[2]),False)
    p[0] = p[1] + p[2]  

def p_expression2(p): # segunda producao
    ''' S : K '''
    p[0] = transformacaoAlfanumerico(int(p[1]),False)

def p_expression3(p):  # terceira producao
    " T : L soma C "
    p[1] = transformacaoAlfanumerico(p[1],True)
    p[0] = p[1] + int(p[3]);

def p_expression4(p):  # quarta producao
    " L : letra "
    p[0] = p[1]

def p_expression5(p):  # quinta producao
    " C : ch "
    p[0] = p[1]

def p_expression6(p):  # sexta producao
    " D : div "
    p[0] = p[1]
   
def p_expression7(p):  # setima producao
    ''' K : T mod D '''
    p[3] = N
    p[0] = str(p[1] %  p[3])

def p_expression8(p):  # oitava producao
    " K : vazio "
    p[0] = -1

# Error na parte sintatica
def p_error(p):
    print("Syntax error in input!")

#--------------------------------------------------------------------------------------------------------

parser = yacc.yacc()
continuar = 1
while continuar:
   mensagemCompleta = ""
   try:
       mensagem = input('Entre com Mensagem para codificar \n')
       chaves = int(input('Entre com inteiro positivo para chave \n'))
       for caracteres in mensagem:
            if(caracteres == " "):
                mensagemCompleta = mensagemCompleta + caracteres
            else:
                mensagemCompleta = mensagemCompleta + caracteres + "+"+ str(chaves) + "%" + "divisor"   
   except EOFError:
       break
   resultado = parser.parse(mensagemCompleta)
   if(resultado):
        print("Mensagem criptografado: "+ '\n' + resultado + "\n")
        continuar = int(input("Digite '0' para sair e qualquer numero para continuar !!! \n"))

