import ply.lex as lex
import ply.yacc as yacc

# Define os tokens
tokens = [
    'INPROGRAMA',
    'FMPROGRAMA',
    'NI',
    'VARIAVEL',
    'INTEIRO',
    'FLOAT',
    'OP_ATRIB_IGUAL',
    'OP_FINAL_LINHA_PONTO_VIRGULA',
    'PARA',
    'LEIA',
    'ESCREVA',
    'ENQUANTO',
    'LESS',
    'GREATER',
    'OP_PAR_ESQUERDO',
    'OP_PAR_DIREITO',
    'OP_CHAVE_ESQUERDA',
    'OP_CHAVE_DIREITA',
    'OP_SOMA',
    'IN',
    'RANGE'
]

# Regras de expressão regular para tokens
t_INPROGRAMA = r'inprograma'
t_FMPROGRAMA = r'fmprograma'
t_NI = r'ni'
t_OP_ATRIB_IGUAL = r'='
t_OP_FINAL_LINHA_PONTO_VIRGULA = r';'
t_PARA = r'para'
t_LEIA = r'leia'
t_ESCREVA = r'escreva'
t_ENQUANTO = r'enquanto'
t_LESS = r'<'
t_GREATER = r'>'
t_OP_PAR_ESQUERDO = r'\('
t_OP_PAR_DIREITO = r'\)'
t_OP_CHAVE_ESQUERDA = r'\{'
t_OP_CHAVE_DIREITA = r'\}'
t_OP_SOMA = r'\+'
t_IN = r'in'
t_RANGE = r'range'

def t_FLOAT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_INTEIRO(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_VARIAVEL(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    if t.value in {'ni', 'inprograma', 'fmprograma', 'escreva', 'leia', 'para', 'enquanto', 'in', 'range'}:
        t.type = t.value.upper()  # Transforma em maiúsculas para tratar como palavra-chave
    return t

t_ignore = ' \t\n'

def t_error(t):
    print("Caractere ilegal '%s'" % t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()

# Conjunto para armazenar variáveis declaradas
variaveis_declaradas = set()
erros = []

# Parsing rules
def p_programa(p):
    '''
    programa : INPROGRAMA declaracoes FMPROGRAMA
    '''
    p[0] = p[2]

def p_declaracoes_single(p):
    '''
    declaracoes : declaracao
    '''
    p[0] = p[1]

def p_declaracoes_mult(p):
    '''
    declaracoes : declaracoes declaracao
    '''
    p[0] = p[1] + p[2]

def p_declaracao_ni_atribuicao(p):
    '''
    declaracao : NI VARIAVEL OP_ATRIB_IGUAL expressao OP_FINAL_LINHA_PONTO_VIRGULA
    '''
    variavel = p[2]
    if variavel in variaveis_declaradas:
        erro = f"Erro: Variável '{variavel}' já foi declarada."
        print(erro)
        erros.append(erro)
        p[0] = ""
    else:
        variaveis_declaradas.add(variavel)
        p[0] = f"{variavel} = {p[4]}\n"

def p_declaracao_atribuicao(p):
    '''
    declaracao : VARIAVEL OP_ATRIB_IGUAL expressao OP_FINAL_LINHA_PONTO_VIRGULA
    '''
    variavel = p[1]
    if variavel not in variaveis_declaradas:
        erro = f"Erro: Variável '{variavel}' não foi declarada antes de usar."
        print(erro)
        erros.append(erro)
        p[0] = ""
    else:
        p[0] = f"{variavel} = {p[3]}\n"

def p_declaracao_escreva(p):
    '''
    declaracao : ESCREVA expressao OP_FINAL_LINHA_PONTO_VIRGULA
    '''
    p[0] = f"print({p[2]})\n"

def p_declaracao_leia(p):
    '''
    declaracao : LEIA VARIAVEL OP_FINAL_LINHA_PONTO_VIRGULA
    '''
    variavel = p[2]
    if variavel not in variaveis_declaradas:
        variaveis_declaradas.add(variavel)
    p[0] = f"{variavel} = float(input('Digite um numero : '))\n"

def p_declaracao_para(p):
    '''
    declaracao : PARA OP_PAR_ESQUERDO VARIAVEL IN RANGE OP_PAR_ESQUERDO expressao OP_PAR_DIREITO OP_PAR_DIREITO OP_CHAVE_ESQUERDA declaracoes OP_CHAVE_DIREITA
    '''
    variavel = p[3]
    variaveis_declaradas.add(variavel)  # Implicitamente declarando a variável do loop
    p[0] = f"for {variavel} in range({p[7]}):\n{indent(p[11], 4)}\n"

def p_declaracao_enquanto(p):
    '''
    declaracao : ENQUANTO OP_PAR_ESQUERDO condicao OP_PAR_DIREITO OP_CHAVE_ESQUERDA declaracoes OP_CHAVE_DIREITA
    '''
    p[0] = f"while {p[3]}:\n{indent(p[6], 4)}\n"

def p_expressao(p):
    '''
    expressao : INTEIRO
              | FLOAT
              | VARIAVEL
              | VARIAVEL OP_SOMA expressao
    '''
    if len(p) == 2:
        p[0] = f"{p[1]}"
    else:
        p[0] = f"{p[1]} + {p[3]}"

def p_condicao(p):
    '''
    condicao : expressao LESS expressao
             | expressao GREATER expressao
    '''
    p[0] = f"{p[1]} {p[2]} {p[3]}"

def p_error(p):
    if p:
        erro = f"Erro de sintaxe em '{p.value}'"
        print(erro)
        erros.append(erro)
    else:
        erro = "Erro de sintaxe no final do arquivo"
        print(erro)
        erros.append(erro)

parser = yacc.yacc()

# Função para adicionar indentação
def indent(text, spaces):
    return '\n'.join(' ' * spaces + line if line else '' for line in text.splitlines())

# Código input
with open('codigo_input.txt', 'r') as file:
    linhas = file.readlines()

codigo_fonte = ""

for linha in linhas:
    codigo_fonte += linha.strip() + "\n"

lexer.input(codigo_fonte)

# Criar arquivo para armazenar os tokens
with open('tokens_output.txt', 'w') as tokens_file:
    while True:
        tok = lexer.token()
        if not tok:
            break
        tokens_file.write(f"Token tipo = {tok.type}, valor = {tok.value}, linha = {tok.lineno}\n")

resultado = parser.parse(codigo_fonte)

# Verificar se houve erros
if erros:
    print("\nErros encontrados:")
    for erro in erros:
        print(erro)
else:
    print("\nAnálise sintática e semântica concluídas com sucesso.")
    print("\nResultado da análise sintática:")
    print(resultado)

    # Código output
    with open('codigo_output.py', 'w') as file:
        file.write(resultado)