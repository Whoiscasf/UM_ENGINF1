#include <stdio.h>
#include <stdlib.h>

typedef struct nodo {
    int valor;
    struct nodo *esq, *dir;
} * ABin;

ABin removeMenor (ABin *a) {
    if (a == NULL) return NULL;
    while ((*a)->esq != NULL) {
        a = &((*a)->esq);
    }
    ABin menor = *a;
    *a = menor->dir;
    return menor;
}

void removeRaiz(ABin *a) {
    if (*a == NULL) return;
    ABin raiz = *a;
    if (raiz->esq == NULL) {
        *a = raiz->dir;
        free(raiz);
    }
    else if (raiz->dir == NULL) {
        *a = raiz->esq;
        free(raiz);
    }
    else {
        ABin pai = raiz;
        ABin menor = raiz->dir;
        while (menor->esq != NULL) {
            pai = menor;
            menor = menor->esq;
        }
        if (pai == raiz) {
            pai->dir = menor->dir;
        } else {
            pai->esq = menor->dir;
        }
        menor->esq = raiz->esq;
        menor->dir = raiz->dir;
        *a = menor;
        free(raiz);
    }
}

int removeElem(ABin *a, int x) {
    while (*a != NULL && (*a)->valor != x) {
        if (x < (*a)->valor) {
            a = &((*a)->esq);
        } else {
            a = &((*a)->dir);
        }
    }
    if (*a == NULL) {
        return 1;
    }
    removeRaiz(a);
    return 0;
}

