#include <stdlib.h>
#include <math.h>
#include "Jogo.h"

//
//
// Lógica de jogo
//
//

// Função que dermina se uma jogada é válida (apenas se o módulo da diferença entre os valores for 1)
int jogadaValida (carta c, carta d) {
    if (abs (c.num - d.num) == 1)
        return 1;
    else
        return 0;
}

// Função que cria um baralho com 52 cartas
void criarBaralho(jogo *j) {
    int i = 0;
    for (int naipe = 0; naipe < 4; naipe++) {
        for (int num = 1; num <= 13; num++) {
            (*j).baralho[i].num = num;
            (*j).baralho[i].naipe = naipe;
            i++;
        }
    }
    (*j).cartas_baralho = 52;
}

// Funçao que baralha as cartas usando o método Fisher-Yates
void baralharCartas(jogo *j) {
    for (int i = 51; i > 0; i--) {
        int r = rand() % (i + 1);
        carta temp = (*j).baralho[i];
        (*j).baralho[i] = (*j).baralho[r];
        (*j).baralho[r] = temp;
    }
}

// Função que distribui as cartas pelas 7 pilhas
void distribuirCartas(jogo *j) {
    for (int p = 0; p < 7; p++) {
        for (int c = 0; c < 5; c++) {
            (*j).cartas_baralho--;
            int topo_do_baralho = (*j).cartas_baralho;
            (*j).pilhas[p][c] = (*j).baralho[topo_do_baralho];
        }
        (*j).quantidade_pilhas[p] = 5;
    }
}

// Função que retira a carta do topo do baralho e coloca-a no descarte para iniciar o jogo
void descarte(jogo *j){
    (*j).cartas_descarte = 0;
    (*j).cartas_baralho--;
    int topo_baralho = (*j).cartas_baralho;

    int topo_descarte = (*j).cartas_descarte;
    (*j).descarte[topo_descarte] = (*j).baralho[topo_baralho];

    (*j).cartas_descarte++;
}

// Função que invoca outras funções necessárias para o começo do jogo
void comecarJogo(jogo *j) {
    criarBaralho(j);
    baralharCartas(j);
    distribuirCartas(j);
    descarte(j);
}

// Função que faz o biscar de uma carta; devolve se este foi bem sucedido ou não
int biscarCarta(jogo *j) {
    if ((*j).cartas_baralho > 0) {
        (*j).cartas_baralho--;
        int topo_baralho = (*j).cartas_baralho;
        int topo_descarte = (*j).cartas_descarte;

        (*j).descarte[topo_descarte] = (*j).baralho[topo_baralho];
        (*j).cartas_descarte++;
        return 1;
    }
    return 0;
}

// Função que verifica a validade da jogada e, se for válida, move a carta da pilha para o descarte, retornando o estado da operação
int moverDaPilha(jogo *j, int opcao) {
    int p = opcao - 1;
    if ((*j).quantidade_pilhas[p] > 0) {
        int topo_p = (*j).quantidade_pilhas[p] - 1;
        int topo_d = (*j).cartas_descarte - 1;
        carta c_pilha = (*j).pilhas[p][topo_p];
        carta c_descarte = (*j).descarte[topo_d];

        if (jogadaValida(c_pilha, c_descarte) == 1) {
            (*j).descarte[topo_d + 1] = c_pilha;
            (*j).cartas_descarte++;
            (*j).quantidade_pilhas[p]--;
            return 1;
        }
        return 0;
    }
    return -1;
}

// Função que verifica a vitória
int verificarVitoria(jogo *j) {
    int pilhas_vazias = 0;
    for (int i = 0; i < 7; i++) {
        if ((*j).quantidade_pilhas[i] == 0) {
            pilhas_vazias++;
        }
    }
    if (pilhas_vazias == 7)
        return 1;
    return 0;
}
