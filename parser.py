import ply.lex as lex
import ply.yacc as yacc


#ANALISE LEXICA

reserved = {
    'int' : 'INT',
    'if' : 'IF',
    'else' : 'ELSE',
    'while' : 'WHILE',
    'print' : 'PRINT',
    'read' : 'READ',
    'output' : 'OUTPUT',
}

tokens = [
'OP_MAT_ADICAO',            #+
'OP_MAT_SUBTRACAO',         #-
'OP_MAT_MULTIPLICACAO',     #*
'OP_MAT_DIVISAO',           #/
'OP_MAT_RESTO',             #%

'ATRIBUICAO',               #=
'OP_NAODETERMINISTICO',     #?

'OP_LOG_MAIOR',             #>
'OP_LOG_MAIORIGUAL',        #>=
'OP_LOG_MENOR',             #>
'OP_LOG_MENORIGUAL',        #>=
'OP_LOG_IGUAL',             #==
'OP_LOG_DIFERENTE',         #!=

'OP_LOG_AND',               #&&
'OP_LOG_OR',                #||
'OP_LOG_NO',                #!

'FINAL_LEXEMA',             #;
'COMENTARIO',               ##
'ASPAS_DUPLAS',             #"

'ABRE_PARENTESES',          #(
'FECHA_PARENTESES',         #)
'ABRE_COLCHETES',           #[  
'FECHA_COLCHETES',          #]
'ABRE_CHAVES',              #{
'FECHA_CHAVES',             #}

'VALOR_INTEIRO',
'VARIAVEL',
'CADEIA_CARACTERES',

'VALOR_INTEIRO_ERRO',
'VARIAVEL_ERRO',
'CADEIA_CARACTERES_ERRO',

'ignore'
]  + list(reserved.values())


#Regras de Expressao Regulares

t_OP_MAT_ADICAO           = r'\+'
t_OP_MAT_SUBTRACAO        = r'-'
t_OP_MAT_MULTIPLICACAO    = r'\*'
t_OP_MAT_DIVISAO          = r'/'
t_OP_MAT_RESTO            = r'\%'

t_ATRIBUICAO              = r'\='
t_OP_NAODETERMINISTICO    = r'\?'

t_OP_LOG_MAIOR            = r'\>'
t_OP_LOG_MAIORIGUAL       = r'\>\='
t_OP_LOG_MENOR            = r'\<'
t_OP_LOG_MENORIGUAL       = r'\<\='
t_OP_LOG_IGUAL            = r'\=\='
t_OP_LOG_DIFERENTE        = r'\!\='

t_OP_LOG_AND              = r'\&\&'
t_OP_LOG_OR               = r'\|\|'
t_OP_LOG_NO               = r'\!'

t_FINAL_LEXEMA           = r'\;'
t_COMENTARIO             = r'\#'
t_ASPAS_DUPLAS           = r'\"'

t_ABRE_PARENTESES        = r'\('
t_FECHA_PARENTESES       = r'\)'
t_ABRE_COLCHETES         = r'\['
t_FECHA_COLCHETES        = r'\]'
t_ABRE_CHAVES            = r'\{'
t_FECHA_CHAVES           = r'\}'

t_INT                   = r'int'
t_IF                    = r'if'
t_ELSE                  = r'else'
t_WHILE                 = r'while'
t_PRINT                 = r'print'
t_READ                  = r'read'
t_OUTPUT                = r'output'

t_ignore                = ' \t'

def t_VALOR_INTEIRO(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_VALOR_INTEIRO_ERRO(t):
    r'([0-9]+\.[a-z]+[0-9]+)|([0-9]+\.[a-z]+)|([0-9]+\.[0-9]+[a-z]+)'
    return t

def t_VARIAVEL(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    if t.value in reserved:
        t.type = reserved[t.value]
    return t

def t_VARIAVEL_ERRO(t):
    r'([0-9]+[a-z]+)|([@!#$%&*]+[a-z]+|[a-z]+\.[0-9]+|[a-z]+[@!#$%&*]+)'
    return t

def t_CADEIA_CARACTERES(t):
    r'("[^"]*")'
    return t

def t_CADEIA_CARACTERES_ERRO(t):
    r'("[^"]*)'
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print("Caractere invalido")
    t.lexer.skip(1)




#ANALISE SINTATICA

def p_statement(p):
    '''
    statement   :   criacaoVariavel
                |   atribuicaoValor
                |   fechamentoEscopo
    '''
    p[0]=p[1]

def p_criacaoVariavel(p):
    '''
    criacaoVariavel :   INT VARIAVEL FINAL_LEXEMA
    '''
    print("criou variavel")

def p_atribuicaoValor(p):
    '''
    atribuicaoValor :   VARIAVEL ATRIBUICAO valorAtribuido
    '''
    print("atribui valor")

def p_valorAtribuido(p):
    '''
    valorAtribuido  :   expressaoMatematica
                    |   expressaoMatematica OP_NAODETERMINISTICO expressaoMatematica
    '''
    print("valor atribuido")

def p_expressaoMatematica(p):
    AAAAAAAAa

def p_chamadaFuncao(p):
    pass

def p_fechamentoEscopo(p):
    '''
    fechamentoEscopo    :   FECHA_CHAVES
    '''
    print("fechou escopo")

def p_escritaLeitura(p):
    pass

errosSintaticos = []
def p_error(p):
    errosSintaticos.append(p)
    print("ERRO: ",p)

#dadoAnalisado = '''
#56 + 96¬;
#if - while ([ghj]){}for
#output.
#'''

dadoAnalisado = '''
    int teste;
    aaaa{{
    }
'''

print(dadoAnalisado)

lexer = lex.lex()
lexer.input(dadoAnalisado)

#while True:
#    tok = lexer.token()
#    if not tok:
#        break
#    print(tok)

parser = yacc.yacc()    
resultado = parser.parse(dadoAnalisado)

print(resultado)

