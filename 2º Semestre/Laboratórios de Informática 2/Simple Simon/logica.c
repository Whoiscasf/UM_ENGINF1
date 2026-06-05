#include <stdlib.h>
#include "Jogo.h"

// Função que cria um baralho com 52 cartas (4 naipes, 13 valores)
void criarBaralho(jogo *j) {
    int i = 0;
    for (int naipe = 0; naipe < 4; naipe++) {
        for (int num = 1; num <= 13; num++) {
            (*j).baralho[i].num = num;
            (*j).baralho[i].naipe = naipe;
            i++;
        }
    }
}

// Função que baralha as cartas usando o metodo Fisher-Yates
void baralharCartas(jogo *j) {
    for (int i = 51; i > 0; i--) {
        int r = rand() % (i + 1);
        carta temp = (*j).baralho[i];
        (*j).baralho[i] = (*j).baralho[r];
        (*j).baralho[r] = temp;
    }
}

// Função que distribui as cartas pelas 10 pilhas com quantidades variáveis
void distribuirCartas(jogo *j) {
    int tam[10] = {8, 8, 8, 7, 6, 5, 4, 3, 2, 1};
    int carta_atual = 51;
    for (int p = 0; p < 10; p++) {
        (*j).quantidade_pilhas[p] = tam[p];
        for (int c = 0; c < tam[p]; c++) {
            (*j).pilhas[p][c] = (*j).baralho[carta_atual];
            carta_atual--;
        }
    }
    (*j).pilhas_concluidas = 0;
}

// Função que invoca as funções necessárias para o começo do jogo
void comecarJogo(jogo *j) {
    criarBaralho(j);
    baralharCartas(j);
    distribuirCartas(j);
}

// Função que verifica se os índices de origem, destino e quantidade são válidos
int limitesValidos(jogo *j, int o, int d, int q) {
    if (o < 0 || o > 9 || d < 0 || d > 9) return 0;
    if (q <= 0 || q > (*j).quantidade_pilhas[o]) return 0;
    return 1;
}

// Função que verifica se as cartas a mover formam uma sequência válida (mesmo naipe, ordem descendente)
int sequenciaValida(jogo *j, int origem, int qtd) {
    int topo = (*j).quantidade_pilhas[origem] - 1;
    int base_seq = topo - qtd + 1;
    int erro = 1;

    for(int i = base_seq; i < topo; i++) {
        carta c1 = (*j).pilhas[origem][i];
        carta c2 = (*j).pilhas[origem][i+1];
        if (erro == 1) {
            if (c1.naipe != c2.naipe) erro = -2;
            else if (c1.num != c2.num + 1) erro = -3;
        }
    }
    return erro;
}

// Função que verifica se o destino aceita a carta a mover (valor imediatamente superior)
int destinoValido(jogo *j, carta movida, int destino) {
    int dest_q = (*j).quantidade_pilhas[destino];
    if (dest_q == 0) return 1;

    carta topo_dest = (*j).pilhas[destino][dest_q - 1];
    if (topo_dest.num != movida.num + 1) return -4;

    return 1;
}

// Função que valida todos os aspetos de uma jogada, devolvendo o código de erro adequado
int validarJogadaTotal(jogo *j, int o, int d, int q) {
    if (limitesValidos(j, o, d, q) == 0) return -1;

    int seq = sequenciaValida(j, o, q);
    if (seq != 1) return seq;

    carta c = (*j).pilhas[o][(*j).quantidade_pilhas[o] - q];
    return destinoValido(j, c, d);
}

// Função que move as cartas da pilha de origem para a pilha de destino
void moverCartas(jogo *j, int orig, int dest, int qtd) {
    int topo_o = (*j).quantidade_pilhas[orig];
    int base_s = topo_o - qtd;
    int topo_d = (*j).quantidade_pilhas[dest];
    for (int i = 0; i < qtd; i++) {
        (*j).pilhas[dest][topo_d + i] = (*j).pilhas[orig][base_s + i];
    }
    (*j).quantidade_pilhas[orig] -= qtd;
    (*j).quantidade_pilhas[dest] += qtd;
}

// Função que remove uma sequência completa (Rei ao As do mesmo naipe) se existir no topo da coluna
void tentarRemoverSequencia(jogo *j, int col) {
    if ((*j).quantidade_pilhas[col] >= 13) {
        if (sequenciaValida(j, col, 13) == 1) {
            int t = (*j).quantidade_pilhas[col] - 1;
            if ((*j).pilhas[col][t].num == 1) {
                (*j).quantidade_pilhas[col] -= 13;
                (*j).pilhas_concluidas++;
            }
        }
    }
}

// Função que valida e executa um movimento, devolvendo o resultado da operação
int executarMovimento(jogo *j, int o, int d, int q) {
    int validacao = validarJogadaTotal(j, o, d, q);

    if (validacao == 1) {
        moverCartas(j, o, d, q);
        tentarRemoverSequencia(j, d);
    }

    return validacao;
}