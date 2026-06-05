#include <stdio.h>
#include "Jogo.h"

// Função que imprime uma carta com cor consoante o naipe
void imprimirCarta(carta c) {
    const char* naipes[] = {"\u2660", "\u2665", "\u2666", "\u2663"};
    const char* vals[] = {"", "A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"};
    const char* cores[] = {"\033[0m", "\033[31m", "\033[31m", "\033[0m"};

    printf("%s[%s%s]\033[0m\t", cores[c.naipe], vals[c.num], naipes[c.naipe]);
}

// Função que imprime o cabeçalho com o título e as regras do jogo
void mostrarCabecalho(void) {
    printf("\n=================================================================================\n");
    printf(" \033[0m\u2660\033[0m \033[31m\u2665\033[0m   JOGO SIMPLE SIMON   \033[31m\u2666\033[0m \033[0m\u2663\033[0m  \n");
    printf("=================================================================================\n");
    printf(" Regras:\n");
    printf(" - Move cartas/sequencias validas para um valor imediatamente superior.\n");
    printf(" - O objetivo e formar sequencias em ordem decrescente (Rei ao As) do mesmo naipe.\n");
    printf("=================================================================================\n");
}

// Função que imprime as instruções de como realizar uma jogada
void instrucoes(void) {
    printf(" Como jogar:\n");
    printf(" - Primeiro seleciona a coluna de origem.\n");
    printf(" - Depois seleciona a coluna de destino.\n");
    printf(" - Por fim, seleciona o numero de cartas a mover.\n");
}

// Função que mostra o estado atual da mesa, incluindo o número de pilhas concluídas
void mostrarMesa(jogo *j) {
    printf("=== ESTADO ATUAL === \n");
    printf("Pilhas concluidas: %d / 4\n", (*j).pilhas_concluidas);
    printf("---------------------------------------------------------------------------------\n");
}

// Função auxiliar que devolve o número máximo de cartas entre todas as colunas
int obterMaxLinhas(jogo *j) {
    int max = 0;
    for (int i = 0; i < 10; i++) {
        if ((*j).quantidade_pilhas[i] > max) {
            max = (*j).quantidade_pilhas[i];
        }
    }
    return max;
}

// Função que mostra todas as pilhas do jogo em grelha
void mostrarPilhas(jogo *j) {
    int max = obterMaxLinhas(j);

    printf("C1\tC2\tC3\tC4\tC5\tC6\tC7\tC8\tC9\tC10\n\n");

    for (int linha = 0; linha < max; linha++) {
        for (int col = 0; col < 10; col++) {
            if ((*j).quantidade_pilhas[col] > linha) {
                imprimirCarta((*j).pilhas[col][linha]);
            } else {
                printf("\t");
            }
        }
        printf("\n");
    }
    printf("=================================================================================\n");
}

// Função que imprime "GANHASTE"
void vitoria(void) {
    printf("  GGGGG    AAA    N   N  H   H   AAA    SSSSS  TTTTT EEEEE\n");
    printf(" G        A   A   NN  N  H   H  A   A  S        T    E    \n");
    printf(" G  GGG   AAAAA   N N N  HHHHH  AAAAA   SSS     T    EEEE \n");
    printf(" G    G   A   A   N  NN  H   H  A   A      S    T    E    \n");
    printf("  GGGGG   A   A   N   N  H   H  A   A  SSSSS    T    EEEEE\n");
}

// Função que imprime a mensagem de erro correspondente ao código recebido
void imprimirErro(int codigo) {
    if (codigo == -1) printf("\n\n\n\n\n\n\n \033[93m[ERRO]\033[0m Coluna inexistente ou quantidade invalida!\n\n\n\n\n");
    if (codigo == -2) printf("\n\n\n\n\n\n\n \033[93m[ERRO]\033[0m As cartas selecionadas nao sao todas do mesmo naipe!\n\n\n\n\n");
    if (codigo == -3) printf("\n\n\n\n\n\n\n \033[93m[ERRO]\033[0m As cartas selecionadas nao estao ordenadas de forma descendente!\n\n\n\n\n");
    if (codigo == -4) printf("\n\n\n\n\n\n\n \033[93m[ERRO]\033[0m O destino nao e um valor imediatamente superior!\n\n\n\n\n");
}

// Função que imprime o resultado da jogada, sucesso ou erro
void imprimirResultado(int v) {
    if (v == 1) {
        printf("\n\n\n\n \033[1;32m[SUCESSO] Boa jogada!\033[0m\n");
    } else {
        imprimirErro(v);
    }
}

// Função que lê os dados da jogada do utilizador e executa o movimento
int processarJogada(jogo *j) {
    int o, d, q;
    printf(" Origem (1-10) [0 para Sair]: ");
    scanf("%d", &o);
    if (o == 0) {
        printf("\n\n Obrigado por teres jogado Simple Simon.\n\n");
        return 0;
    }
    printf(" Destino (1-10): ");
    scanf("%d", &d);
    printf(" Quantidade de cartas a mover: ");
    scanf("%d", &q);
    imprimirResultado(executarMovimento(j, o - 1, d - 1, q));
    return 1;
}