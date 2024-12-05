import ply.lex as lex
import ply.yacc as yacc

import json
from typing import Literal


#RETORNO DE ERROS

class Erro:
    def __init__(self, mensagem, linha, coluna, _type: Literal['LEXICO', 'SINTATICO', 'SEMANTICO'], dados=None):
        self.mensagem = mensagem
        self.linha = linha
        self.coluna = coluna 
        self.tipoErro = _type
        self.dados = dados

    def __repr__(self):
        return (f"ERRO ({self.tipoErro}): {self.mensagem} na linha {self.linha}, coluna {self.coluna}"+(f". Em:\n{retornaContexto(self.dados, self.linha, self.coluna)}" if self.dados else ""))

    def __eq__(self, outro):
        return (self.mensagem == outro.mensagem and self.linha == outro.linha and self.coluna == outro.coluna and self.tipoErro == outro.tipoErro)

    def retornoSimpes(self):
        return f"Erro ({self.messagem}, linha={self.linha}, coluna={self.coluna}, _type={self.tipoErro})"


def retornaContexto(dados, linha, coluna):
    linhaErro = dados.split('\n')[linha-1]
    onde = linhaErro.lstrip() + '\n' + " " * ((coluna - len('\n'.join(dados.split('\n')[:linha-1])))-((len(linhaErro)-len(linhaErro.lstrip()))+1)) + "^"

    return onde

#ANALISE LEXICA

class Lexer:

    def __init__(self, erros: list[Erro], debug=False):
        self.lex = None
        self.data = None
        self.debug = debug
        self.tabelaSimbolos = tabelaSimbolos()
        self.erros = erros



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

    def t_VALOR_INTEIRO(self, t):
        r'\d+'
        t.value = int(t.value)
        return t

    def t_VALOR_INTEIRO_ERRO(self, t):
        r'([0-9]+\.[a-z]+[0-9]+)|([0-9]+\.[a-z]+)|([0-9]+\.[0-9]+[a-z]+)'
        return t

    def t_VARIAVEL(self, t):
        r'[a-zA-Z_][a-zA-Z_0-9]*'
        if t.value in self.reserved:
            t.type = self.reserved[t.value]
        return t

    def t_VARIAVEL_ERRO(self, t):
        r'([0-9]+[a-z]+)|([@!#$%&*]+[a-z]+|[a-z]+\.[0-9]+|[a-z]+[@!#$%&*]+)'
        return t

    def t_CADEIA_CARACTERES(self, t):
        r'("[^"]*")'
        return t

    def t_CADEIA_CARACTERES_ERRO(self, t):
        r'("[^"]*)'
        return t

    def t_newline(self, t):
        r'\n+'
        t.lexer.lineno += len(t.value)

    def t_error(self, t):
        self.erros.append(Erro("Caractere invalido '%s'" % t.value[0], t.lineno, t.lexpos, 'LEXICO', self.data))
        print(self.erros[-1])
        t.lexer.skip(1)

    def build(self):
        self.lex = lex.lex(module=self)



#ANALISE SINTATICA


class Parser:
    tokens = Lexer.tokens

    def __init__(self, debug=False):
        self.erros: list[Erro] = []
        self.parser = None
        self.data = None
        self.debug = debug
        self.lexer = Lexer(self.erros, debug=self.debug)

    def verificaExistencia(self, simbolo, p):
        if not self.lexer.tabelaSimbolos.verificaExistenciaSimbolo(simbolo):
            self.erros.append(Erro(f"Simbolo '{simbolo}' nao declarado", p.lexer.lineno, p.lexer.lexpos, 'SEMANTICO', self.data))
            print(self.erros[-1])

    def passaParser(self, data):
        self.data = data
        self.lexer.data = data
        self.parser.parse(data, tracking=True)

    def build(self, build_lexer=False):
        self.parser = yacc.yacc(module=self)
        if build_lexer:
            self.lexer.build()


    def p_program(self, p):
        '''
        program     :   statement program
                    |   statement
        '''

    def p_statement(self, p):
        '''
        statement   :   criacaoVariavel
                    |   atribuicaoValor
                    |   chamadaFuncao
                    |   leituraEscrita
                    |   defineOutput
                    |   fechamentoEscopo
        '''
        
    def p_criacaoVariavel(self, p):
        '''
        criacaoVariavel :   INT VARIAVEL FINAL_LEXEMA
        '''
        print("criou variavel")

        
        if not self.lexer.tabelaSimbolos.verificaExistenciaSimbolo(p[2]):
            self.lexer.tabelaSimbolos.adicionaSimbolo(p[2], None)
        else:
            self.erros.append(Erro(f"Simbolo '{p[2]}' ja declarado", p.lexer.lineno, p.lexer.lexpos, 'SEMANTICO', self.data))
            print(self.erros[-1])
                

    def p_atribuicaoValor(self, p):
        '''
        atribuicaoValor :   VARIAVEL ATRIBUICAO valorAtribuido FINAL_LEXEMA
        '''
        print("atribui valor")
        if self.lexer.tabelaSimbolos.verificaExistenciaSimbolo(p[1]):
            self.lexer.tabelaSimbolos.atualizaSimbolo(p[1], p[3])
        else:
            self.erros.append(Erro(f"Simbolo '{p[1]}' ainda nao declarado", p.lexer.lineno, p.lexer.lexpos, 'SEMANTICO', self.data))
            print(self.erros[-1])

    def p_valorAtribuido(self, p):
        '''
        valorAtribuido  :   expressaoMatematica
                        |   expressaoMatematica OP_NAODETERMINISTICO expressaoMatematica
        '''
        #print("valor atribuido")

    def p_expressaoMatematica(self, p):
        '''
        expressaoMatematica :   operando
                            |   ABRE_PARENTESES expressaoMatematica FECHA_PARENTESES
                            |   expressaoMatematica operadorMatematico expressaoMatematica
        '''
        ##print("expressao matematica")

    def p_operadorMatematico(self, p):
        '''
        operadorMatematico  :   OP_MAT_ADICAO
                            |   OP_MAT_SUBTRACAO
                            |   OP_MAT_MULTIPLICACAO
                            |   OP_MAT_DIVISAO
                            |   OP_MAT_RESTO
        '''

    def p_operando(self, p):
        '''
        operando    :   VARIAVEL
                    |   VALOR_INTEIRO   
        '''

    def p_chamadaFuncao(self, p):
        '''
        chamadaFuncao   :   WHILE ABRE_PARENTESES chamadaOperacaoLogica FECHA_PARENTESES ABRE_CHAVES
                        |   IF ABRE_PARENTESES chamadaOperacaoLogica FECHA_PARENTESES ABRE_CHAVES
                        |   ELSE ABRE_CHAVES
        '''
        print("chamou funcao")

    def p_chamadaOperacaoLogica(self, p):
        '''
        chamadaOperacaoLogica   :   ABRE_PARENTESES operacaoLogica FECHA_PARENTESES portaLogica chamadaOperacaoLogica
                                |   ABRE_PARENTESES operacaoLogica FECHA_PARENTESES
                                |   operacaoLogica
        '''

    def p_operacaoLogica(self, p):
        '''
        operacaoLogica  :   OP_LOG_NO ABRE_PARENTESES operacaoLogica FECHA_PARENTESES
                        |   expressaoMatematica operadorLogico expressaoMatematica
        '''

    def p_operadorLogico(self, p):
        '''
        operadorLogico  :   OP_LOG_MAIOR
                        |   OP_LOG_MAIORIGUAL
                        |   OP_LOG_MENOR
                        |   OP_LOG_MENORIGUAL
                        |   OP_LOG_IGUAL
                        |   OP_LOG_DIFERENTE
        '''

    def p_portaLogica(self, p):
        '''
        portaLogica :   OP_LOG_AND
                    |   OP_LOG_OR
        '''

    def p_leituraEscrita(self, p):
        '''
        leituraEscrita  :   PRINT ABRE_PARENTESES CADEIA_CARACTERES FECHA_PARENTESES FINAL_LEXEMA
                        |   PRINT ABRE_PARENTESES VARIAVEL FECHA_PARENTESES FINAL_LEXEMA
                        |   READ ABRE_PARENTESES VARIAVEL FECHA_PARENTESES FINAL_LEXEMA  
        '''
        print("leitura/escrita")

    def p_defineOutput(self, p):
        '''
        defineOutput    :   OUTPUT ABRE_PARENTESES VALOR_INTEIRO FECHA_PARENTESES FINAL_LEXEMA
        '''
        print("output")

    def p_fechamentoEscopo(self, p):
        '''
        fechamentoEscopo    :   FECHA_CHAVES
        '''
        print("fechou escopo")

    def p_error(self, p):
        if not p:
            self.erros.append(Erro("Fim de input inesperado", 0, 0, 'SINTATICO', self.data))
        else:
            self.erros.append(Erro(f"Erro sintatico em '{p.value}", p.lineno, p.lexpos, 'SINTATICO', self.data))




#ANALISE SEMANTICA

class tabelaSimbolos:

    def __init__(self):
        self.tabela = {}

    def adicionaSimbolo(self, simbolo, valor):
        if simbolo not in self.tabela:
            self.tabela[simbolo] = {"Valor": valor}
        else: 
            raise Exception("Variavel ja declarada")

    def removeSimbolo(self, simbolo):
        del self.tabela[simbolo]

    def atualizaSimbolo(self, simbolo, valor):
        if simbolo in self.tabela:
            self.tabela[simbolo] = {"Valor": valor}
        else: 
            raise Exception("Variavel ainda nao declarada")

    def verificaExistenciaSimbolo(self, simbolo):
        return simbolo in self.tabela
    
    def retornaSimbolo(self, simbolo):
        if simbolo in self.tabela:
            return self.table[simbolo]
        else: 
            raise Exception("Variável ainda não declarada")   


dadoAnalisado = '''
    int valor;
    int cont;
    cont = 0;
    
    teste = 5;

    while ((cont<5)){
        valor = cont * cont;
        print(valor);
    }

'''

print(dadoAnalisado)

#lexer = lex.lex()
#lexer.input(dadoAnalisado)

#while True:
#    tok = lexer.token()
#    if not tok:
#        break
#    print(tok)

#parser = yacc.yacc()    
#resultado = parser.parse(dadoAnalisado)

#print(resultado)

parser = Parser(debug=False)
parser.build(build_lexer=True)

parser.passaParser(dadoAnalisado)

print(parser.lexer.tabelaSimbolos.tabela)



