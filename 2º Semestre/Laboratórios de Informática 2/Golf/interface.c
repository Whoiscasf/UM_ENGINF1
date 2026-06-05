#include <stdio.h>
#include "Jogo.h"

//
//
// Interface gráfica do jogo
//
//

// Função para imprimir uma carta
void imprimirCarta( carta c) {
    const char* naipes[] = {"\u2660", "\u2665", "\u2666", "\u2663"}; // Imprime o naipe
    const char* valores[] = {"", "A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"}; // Imprime o valor; tem o valor vazio porque o array começa em 0
    printf("[%s%s] ", valores[c.num], naipes[c.naipe]);
}

// Função para imprimir um cabeçalho com as regras de jogo
void mostrarCabecalho(void) {
    printf("\n============================================\n");
    printf("\u2660 \u2665   BEM-VINDO AO JOGO DE CARTAS GOLF   \u2666 \u2663  \n");
    printf("============================================\n");
    printf(" REGRAS:\n");
    printf(" - Retira cartas das 7 pilhas passando-as para o descarte.\n");
    printf(" - A carta a mover tem de ser 1 valor acima ou abaixo do valor da carta do descarte.\n");
    printf(" - Podes retirar uma carta do baralho a qualquer momento.\n");
    printf("============================================\n");
}

// Função que mostra a mesa de jogo (baralho e pilha de descarte)
void mostrarMesa(jogo *j) {
    printf("=== MESA ATUAL === \n");
    printf("BARALHO: %d cartas no baralho \n", (*j).cartas_baralho);
    printf("DESCARTE (Carta visivel): ");

    if ((*j).cartas_descarte > 0) {
        int topo = (*j).cartas_descarte - 1;
        imprimirCarta((*j).descarte[topo]); // Mostra a carta do topo
    } else {
        printf("[VAZIO]");
    }
    printf("\n--------------------------------------------\n");
}

// Função que mostra as 7 pilhas do jogo
void mostrarPilhas(jogo *j) {
    for (int i = 0; i < 7; i++) {
        printf("%dª pilha: ", i + 1);

        if ((*j).quantidade_pilhas[i] > 0) {
            int topo = (*j).quantidade_pilhas[i] - 1;
            imprimirCarta((*j).pilhas[i][topo]);
            printf("  (restam %d na pilha)\n", (*j).quantidade_pilhas[i]);
        } else {
            printf(" [VAZIA]\n");
        }
    }
    printf("============================================\n");
}

// Função que imprime "GANHASTE" quando há uma vitoria; funciona como um 'Easter egg' do jogo
void vitoria(void) {
    printf("  GGGGG    AAA    N   N  H   H   AAA    SSSSS  TTTTT  EEEEE\n");
    printf(" G        A   A   NN  N  H   H  A   A  S        T    E    \n");
    printf(" G  GGG   AAAAA   N N N  HHHHH  AAAAA   SSS     T    EEEE \n");
    printf(" G    G   A   A   N  NN  H   H  A   A      S    T    E    \n");
    printf("  GGGGG   A   A   N   N  H   H  A   A  SSSSS    T    EEEEE\n");
}

// Função que pergunta ao jogador, o que este quer fazer
int pedirOpcao(void) {
    int opcao;
    printf("\n Que jogada queres fazer?\n");
    printf(" [1 a 7] - Mover da Pilha | [0] - Pegar uma carta | [-1] - Sair do jogo\n");
    printf(" Opcao: ");
    scanf("%d", &opcao);
    return opcao;
}

// Função que executa a ação de biscar uma carta e imprime o resultado da operação
void executarBiscar(jogo *j) {
    if (biscarCarta(j) == 1) {
        printf("\n [SUCESSO] Biscaste uma carta nova!\n\n\n\n");
    } else {
        printf("\n [AVISO] O baralho esta vazio!\n\n\n\n");
    }
}

// Função que tenta mover a carta escolhida para o descarte e imprime o sucesso ou erro da jogada
void executarMoverPilha(jogo *j, int opcao) {
    int res = moverDaPilha(j, opcao);
    if (res == 1) {
        printf("\n [SUCESSO] Boa jogada!\n\n\n\n");
    } else if (res == 0) {
        printf("\n [ERRO] Valores nao consecutivos!\n\n\n\n");
    } else {
        printf("\n [AVISO] Essa pilha ja esta vazia!\n\n\n\n");
    }
}

// Função que processa a jogada
int processarJogada(jogo *j) {
    int opcao = pedirOpcao();
    if (opcao == -1) {
        printf("\n Obrigado por teres jogado Golf\n Até à próxima :)\n O JOGO TERMINOU\n");
        return 0;
    }
    if (opcao == 0) {
        executarBiscar(j);
        return 1;
    }
    if (opcao >= 1 && opcao <= 7) {
        executarMoverPilha(j, opcao);
        return 1;
    }
    printf("\n [ERRO] Opcao invalida!\n");
    return 1;
}
