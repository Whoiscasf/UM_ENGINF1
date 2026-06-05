#include <stdio.h>
#include <stdlib.h>

typedef struct nodo {
    int valor;
    struct nodo *esq, *dir;
} * ABin;

ABin newABin (int r, ABin e, ABin d) {
    ABin a = malloc (sizeof(struct nodo));
    if (a != NULL) {
        a->valor = r;
        a->esq = e;
        a->dir = d;
    }
    return a;
}

int altura (ABin a) {
    if (a == NULL) return 0;
    int alturaEsq = altura(a->esq);
    int alturaDir = altura(a->dir);
    if (alturaEsq > alturaDir) {
        return 1 + alturaEsq;
    } else {
        return 1 + alturaDir;
    }
}

int nFolhas(ABin a) {
    if (a == NULL) return 0;
    if (a->esq == NULL && a->dir == NULL)
        return 1;
    return (nFolhas(a->esq) + nFolhas(a->dir));
}

ABin maisEsquerda (ABin a) {
    if (a == NULL) return NULL;
    if (a->esq == NULL) return a;
    return (maisEsquerda(a->esq));
}

void imprimeNivel (ABin a, int l) {
    if (a == NULL) return;
    if (l == 0) {
        printf("%d", a->valor);
    } else {
        imprimeNivel(a->esq, l-1);
        imprimeNivel(a->dir, l-1);
    }
}

int procuraE (ABin a, int x) {
    if (a == NULL) return 0;
    if (a->valor == x) {
        return 1;
    }
    return (procuraE (a->esq, x) || procuraE(a->dir, x));
}

struct nodo *procura(ABin a, int x) {
    if (a == NULL) return NULL;
    if (a->valor == x) {
        return a;
    }
    if (a->valor > x) {
        return procura(a->esq,x);
    } else if (a->valor < x) {
        return procura(a->dir,x);
    } else {
        return NULL;
    }
}

int nivel (ABin a, int x) {
    if (a == NULL) return -1;
    if (a->valor == x) return 0;
    int n;
    if (a->valor > x) {
        n = nivel(a->esq, x);
    } else {
        n = nivel(a->dir, x);
    }
    if (n == -1) {
        return -1;
    }
    return 1 + n;
}

