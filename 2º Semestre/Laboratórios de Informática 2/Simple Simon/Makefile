# 1. Compila o jogo principal no ficheiro "golf"
jogo: main.c logica.c interface.c
	gcc main.c logica.c interface.c -o ss -lm

# 2. Compila e gera o executável dos testes CUnit
testes: testes.c logica.c
	gcc testes.c logica.c -o testes -lcunit -lm

# 3. Corre o teste de complexidade
pmccabe:
	pmccabe -v logica.c interface.c main.c

# 4. Limpa a pasta apagando os programas antigos
clean:
	rm -f ss testes
