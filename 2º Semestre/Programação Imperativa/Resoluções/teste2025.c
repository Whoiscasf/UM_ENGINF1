#include <stdio.h>;
#include <stdlib.h>

// Exercício 1
typedef struct {
    int inf,sup;
} Intervalo;

void ordena(Intervalo c[], int N) {
    int i=0;
    while (i<N) {
        for (int j = 0; j < N-1; j++) {
            if (c[j].inf>c[j+1].inf) {
                Intervalo temp = c[j];
                c[j] = c[j+1];
                c[j+1] = temp;
            }
        }
        i++;
    }
}

int cardinalidade (Intervalo c[], int N) {
    int a[N] = [0];
    for (int i = 0; i < N-1; i++) {

    }

}

void camel2snake(char *id) {
    int i = 0;
    while (id[i] != '\0') {
        if (id[i] >= 'A' && id[i] <= 'Z') {
            int len = strlen(id);
            for (int j = len; j >= i; j--) {
                id[j + 1] = id[j];
            }

            id[i + 1] = id[i + 1] + 32;
            id[i] = '_';
            i++;
        }
        i++;
    }
}

typedef struct no {
    int valor;
    struct no *prox;
} *LInt;

LInt arrayToList(int v[], int N) {
    if (N == 0) {
        return NULL;
    }
    LInt nodo = malloc(sizeof(struct no));
    nodo->valor = v[0];
    nodo->prox = arrayToList(v + 1, N - 1);
    return nodo;
}

int apagaUltimo(LInt *l, int x) {
    LInt atual = *l;
    LInt antAtual = NULL;
    LInt alvo = NULL;
    LInt antAlvo = NULL;
    while (atual != NULL) {
        if (atual->valor == x) {
            alvo = atual;
            antAlvo = antAtual;
        }
        antAtual = atual;
        atual = atual->prox;
    }
    if (alvo == NULL) {
        return 1;
    }
    if (antAlvo == NULL) {
        *l = alvo->prox;
    }
    else {
        antAlvo->prox = alvo->prox;
    }
    free(alvo);
    return 0;
}

typedef struct nodo {
    int valor;
    struct nodo *esq, *dir;
} *ABin;

int calculaDistancia (ABin a, int x) {
    if (a->valor == x) return 0;
    else if (a->valor < x) return 1 + calculaDistancia(a->esq, x);
    else  return 1 + calculaDistancia(a->dir, x);
}

int parentesco (ABin a, int x, int y) {
    if (a == NULL) return -1;
    else if (a->valor < x && a->valor < x) {
        return parentesco(a->esq, x, y);
    } else if (a->valor > x && a->valor > x) {
        return parentesco (a->dir, x,y);
    } else {
        return calculaDistancia(a, x) + calculaDistancia(a, y);
    }
}

