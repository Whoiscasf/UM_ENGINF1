#include <stdlib.h>
#include <stdio.h>

typedef struct lligada {
    int valor;
    struct lligada *prox;
} *LInt;

// Exercício 1

int length (LInt l){
    int n=0;
    while (l != NULL){
        n++;
        l = l->prox;
    }
    return n;
}

// Exercício 2

void freeL (LInt l){
    LInt temp;
    while (l != NULL){
        temp = l->prox;
        free(l);
        l=temp;
    }
}

// Exercício 3

void imprimeL (LInt l){
    while (l != NULL){
        printf("%d\n", l->valor);
        l = l->prox;
    }
}

// Exercício 4

LInt reverseL (LInt l){
    LInt ant = NULL;
    LInt atual = l;
    LInt prox = NULL;
    while (atual != NULL) {
        prox = atual->prox;
        atual->prox = ant;
        ant = atual;
        atual = prox;
    }
    return ant;
}


// Exercício 5

void insertOrd (LInt *l, int x) {
    LInt novo = malloc(sizeof(struct lligada));
    novo->valor = x;
    LInt ant = NULL;
    LInt atual = *l;
    while (atual != NULL && atual->valor < x) {
        ant = atual;
        atual = atual->prox;
    }
    novo->prox = atual;
    if (ant == NULL) {
        *l = novo;
    } else {
        ant->prox = novo;
    }
}

// Exercício 6

int removeOneOrd (LInt *l, int x) {
    LInt ant = NULL;
    LInt atual = *l;
    while (atual != NULL && atual->valor < x) {
        ant = atual;
        atual = atual->prox;
    }
    if (atual == NULL || atual->valor != x) {
        return 1;
    }
    if (ant == NULL) {
        *l = atual->prox;
    } else {
        ant->prox = atual->prox;
    }
    free(atual);
    return 0;
}
















// Exercício 7
void merge (LInt *r, LInt a, LInt b) {
    if (a == NULL) { *r = b; return; }
    if (b == NULL) { *r = a; return; }
    if (a->valor < b->valor) {
        *r = a;
        a = a->prox;
    } else {
        *r = b;
        b = b->prox;
    }
    LInt atual = *r;
    while (a != NULL && b != NULL) {
        if (a->valor < b->valor) {
            atual->prox = a;
            a = a->prox;
        } else {
            atual->prox = b;
            b = b->prox;
        }
        atual = atual->prox;
    }
    if (a != NULL) atual->prox = a;
    else atual->prox = b;
}

// Exercício 8
void splitQS (LInt l, int x, LInt *mx, LInt *Mx) {
    *mx = NULL;
    *Mx = NULL;
    LInt cauda_mx = NULL;
    LInt cauda_Mx = NULL;
    while (l != NULL) {
        if (l->valor < x) {
            if (*mx == NULL) {
                *mx = l;
                cauda_mx = l;
            } else {
                cauda_mx->prox = l;
                cauda_mx = l;
            }
        } else {
            if (*Mx == NULL) {
                *Mx = l;
                cauda_Mx = l;
            } else {
                cauda_Mx->prox = l;
                cauda_Mx = l;
            }
        }
        l = l->prox;
    }
    if (cauda_mx != NULL) cauda_mx->prox = NULL;
    if (cauda_Mx != NULL) cauda_Mx->prox = NULL;
}
// Exercício 9

LInt parteAmeio (LInt *l) {
    int tamanho = 0;
    LInt temp = *l;
    while (temp != NULL) {
        tamanho++;
        temp = temp->prox;
    }
    int meio = tamanho / 2;
    if (meio == 0) return NULL;
    LInt metade = *l;
    LInt ant = NULL;
    temp = *l;
    for (int i = 0; i < meio; i++) {
        ant = temp;
        temp = temp->prox;
    }
    ant->prox = NULL;
    *l = temp;
    return metade;
}

// Exercício 10

int removeAll (LInt *l, int x) {
    int removidos = 0;
    LInt ant = NULL;
    LInt atual = *l;

    while (atual != NULL) {
        if (atual->valor == x) {
            LInt a_remover = atual;
            removidos++;

            if (ant == NULL) {
                *l = atual->prox;
                atual = *l;
            } else {
                ant->prox = atual->prox;
                atual = atual->prox;
            }
            free(a_remover);
        } else {
            ant = atual;
            atual = atual->prox;
        }
    }
    return removidos;
}





// Exercício 11

int removeDups (LInt *l) {
    int removidos = 0;
    LInt atual = *l;
    while (atual != NULL) {
        LInt ant = atual;
        LInt seg = atual->prox;
        while (seg != NULL) {
            if (seg->valor == atual->valor) {
                removidos++;
                LInt a_remover = seg;
                ant->prox = seg->prox;
                seg = ant->prox;
                free(a_remover);
            } else {
                ant = seg;
                seg = seg->prox;
            }
        }
        atual = atual->prox;
    }
    return removidos;
}

// Exercício 12

int removeMaiorL (LInt *l) {
    LInt ant = NULL;
    LInt atual = *l;
    LInt antMaior = NULL;
    LInt maior = *l;
    while (atual != NULL) {
        if (atual->valor > maior->valor) {
            maior = atual;
            antMaior = ant;
        }
        ant = atual;
        atual = atual->prox;
    }
    int valorMax = maior->valor;
    if (antMaior == NULL) {
        *l = maior->prox;
    } else {
        antMaior->prox = maior->prox;
    }
    free(maior);
    return valorMax;
}






// Exercício 13

void init (LInt *l) {
    LInt ant = NULL;
    LInt atual = *l;
    while (atual->prox != NULL) {
        ant = atual;
        atual = atual->prox;
    }
    if (ant == NULL) {
        *l = NULL;
    } else {
        ant->prox = NULL;
    }
    free(atual);
}

// Exercício 14

void appendL (LInt *l, int x) {
    LInt novo = malloc(sizeof(struct lligada));
    novo->valor = x;
    novo->prox = NULL;
    if (*l == NULL) {
        *l = novo;
    } else {
        LInt atual = *l;
        while (atual->prox != NULL) {
            atual = atual->prox;
        }
        atual->prox = novo;
    }
}

// Exercício 15

void concatL (LInt *a, LInt b) {
    if (*a == NULL) {
        *a = b;
    } else {
        LInt atual = *a;
        while (atual->prox != NULL) {
            atual = atual->prox;
        }
        atual->prox = b;
    }
}








// Exercício 16

LInt cloneL (LInt l) {
    LInt nova = NULL;
    LInt *cauda = &nova;
    
    while (l != NULL) {
        *cauda = malloc(sizeof(struct lligada));
        (*cauda)->valor = l->valor;
        (*cauda)->prox = NULL;
        cauda = &((*cauda)->prox);
        l = l->prox;
    }
    return nova;
}

// Exercício 17

LInt cloneRev (LInt l) {
    LInt nova = NULL;
    
    while (l != NULL) {
        LInt novo_no = malloc(sizeof(struct lligada));
        novo_no->valor = l->valor;
        novo_no->prox = nova;
        nova = novo_no;
        l = l->prox;
    }
    return nova;
}

// Exercício 18

int maximo (LInt l) {
    int max = l->valor;
    
    while (l != NULL) {
        if (l->valor > max) {
            max = l->valor;
        }
        l = l->prox;
    }
    return max;
}












// Exercício 19

int take (int n, LInt *l) {
    int comp = 0;
    
    while (*l != NULL && n > 0) {
        l = &((*l)->prox);
        n--;
        comp++;
    }
    
    while (*l != NULL) {
        LInt temp = *l;
        *l = (*l)->prox;
        free(temp);
    }
    return comp;
}

// Exercício 20

int drop (int n, LInt *l) {
    int removidos = 0;
    
    while (*l != NULL && n > 0) {
        LInt temp = *l;
        *l = (*l)->prox;
        free(temp);
        n--;
        removidos++;
    }
    return removidos;
}

// Exercício 21

LInt Nforward (LInt l, int N) {
    while (N > 0) {
        l = l->prox;
        N--;
    }
    return l;
}

// Exercício 22
int listToArray (LInt l, int v[], int N) {
    int i = 0;
    while (l != NULL && i < N) {
        v[i] = l->valor;
        l = l->prox;
        i++;
    }
    return i;
}

// Exercício 23

LInt arrayToList (int v[], int N) {
    LInt nova = NULL;
    LInt *cauda = &nova;
    
    for (int i = 0; i < N; i++) {
        *cauda = malloc(sizeof(struct lligada));
        (*cauda)->valor = v[i];
        (*cauda)->prox = NULL;
        cauda = &((*cauda)->prox);
    }
    return nova;
}

// Exercício 24

LInt somasAcL (LInt l) {
    LInt nova = NULL;
    LInt *cauda = &nova;
    int soma = 0;
    
    while (l != NULL) {
        soma += l->valor;
        *cauda = malloc(sizeof(struct lligada));
        (*cauda)->valor = soma;
        (*cauda)->prox = NULL;
        cauda = &((*cauda)->prox);
        l = l->prox;
    }
    return nova;
}

// Exercício 25

void remreps (LInt l) {
    if (l == NULL) return;
    
    while (l->prox != NULL) {
        if (l->valor == l->prox->valor) {
            LInt temp = l->prox;
            l->prox = temp->prox;
            free(temp);
        } else {
            l = l->prox;
        }
    }
}






// Exercício 26

LInt rotateL (LInt l) {
    if (l == NULL || l->prox == NULL) return l;
    
    LInt nova_cabeca = l->prox;
    LInt temp = nova_cabeca;
    
    while (temp->prox != NULL) {
        temp = temp->prox;
    }
    
    temp->prox = l;
    l->prox = NULL;
    
    return nova_cabeca;
}

// Exercício 27

LInt parte (LInt l) {
    LInt y = NULL;
    LInt *cauda_y = &y;
    
    while (l != NULL && l->prox != NULL) {
        *cauda_y = l->prox;
        l->prox = l->prox->prox;
        l = l->prox;
        cauda_y = &((*cauda_y)->prox);
    }
    *cauda_y = NULL;
    
    return y;
}





















typedef struct nodo {
    int valor;
    struct nodo *esq, *dir;
} *ABin;

// Exercício 28

int altura (ABin a) {
    if (a == NULL) return 0;
    int altEsq = altura(a->esq);
    int altDir = altura(a->dir);
    if (altEsq > altDir) {
        return 1 + altEsq;
    } else {
        return 1 + altDir;
    }
}

// Exercício 29

ABin cloneAB (ABin a) {
    if (a == NULL) return NULL;
    ABin novo = malloc(sizeof(struct nodo));
    novo->valor = a->valor;
    novo->esq = cloneAB(a->esq);
    novo->dir = cloneAB(a->dir);
    return novo;
}

// Exercício 30

void mirror (ABin *a) {
    if (*a != NULL) {
        ABin temp = (*a)->esq;
        (*a)->esq = (*a)->dir;
        (*a)->dir = temp;
        mirror(&((*a)->esq));
        mirror(&((*a)->dir));
    }
}

// Exercício 31
void inorder (ABin a, LInt *l) {
    if (a == NULL) {
        *l = NULL;
    } else {
        inorder(a->esq, l);
        while (*l != NULL) {
            l = &((*l)->prox);
        }
        *l = malloc(sizeof(struct lligada));
        (*l)->valor = a->valor;
        inorder(a->dir, &((*l)->prox));
    }
}
// Exercício 32

void preorder (ABin a, LInt *l) {
    if (a == NULL) {
        *l = NULL;
    } else {
        *l = malloc(sizeof(struct lligada));
        (*l)->valor = a->valor;
        preorder(a->esq, &((*l)->prox));
        while (*l != NULL) {
            l = &((*l)->prox);
        }
        preorder(a->dir, l);
    }
}

// Exercício 33

void posorder (ABin a, LInt *l) {
    if (a == NULL) {
        *l = NULL;
    } else {
        posorder(a->esq, l);
        while (*l != NULL) l = &((*l)->prox);
        posorder(a->dir, l);
        while (*l != NULL) l = &((*l)->prox);
        *l = malloc(sizeof(struct lligada));
        (*l)->valor = a->valor;
        (*l)->prox = NULL;
    }
}

// Exercício 34

int depth (ABin a, int x) {
    if (a == NULL) return -1;
    if (a->valor == x) return 1;
    int esq = depth(a->esq, x);
    int dir = depth(a->dir, x);
    if (esq == -1 && dir == -1) return -1;
    if (esq == -1) return 1 + dir;
    if (dir == -1) return 1 + esq;
    if (esq < dir) {
        return 1 + esq;
    } else {
        return 1 + dir;
    }
}







// Exercício 35

int freeAB (ABin a) {
    if (a == NULL) return 0;
    int apagadosEsq = freeAB(a->esq);
    int apagadosDir = freeAB(a->dir);
    free(a);
    return 1 + apagadosEsq + apagadosDir;
}

// Exercício 36

int pruneAB (ABin *a, int l) {
    int cont = 0;
    if (*a == NULL) return 0;
    if (l == 0) {
        cont += pruneAB(&((*a)->esq), 0);
        cont += pruneAB(&((*a)->dir), 0);
        free(*a);
        *a = NULL;
        return cont + 1;
    }
    else {
        cont += pruneAB(&((*a)->esq), l - 1);
        cont += pruneAB(&((*a)->dir), l - 1);
        
        return cont;
    }
}

// Exercício 37

int iguaisAB (ABin a, ABin b) {
    if (a == NULL && b == NULL) return 1;
    if (a == NULL || b == NULL) return 0;
    if (a->valor == b->valor) {
        return iguaisAB(a->esq, b->esq) && iguaisAB(a->dir, b->dir);
    } else {
        return 0;
    }
}














// Exercício 38

LInt nivelL (ABin a, int n) {
    if (a == NULL || n < 1) {
        return NULL;
    }
    if (n == 1) {
        LInt novo = malloc(sizeof(struct lligada));
        novo->valor = a->valor;
        novo->prox = NULL;
        return novo;
    }
    LInt esq = nivelL(a->esq, n - 1);
    LInt dir = nivelL(a->dir, n - 1);
    if (esq == NULL) {
        return dir;
    }
    LInt temp = esq;
    while (temp->prox != NULL) {
        temp = temp->prox;
    }
    temp->prox = dir;
    return esq;
}


// Exercício 39

int nivelV (ABin a, int n, int v[]) {
    if (a == NULL || n < 1) return 0;
    if (n == 1) {
        v[0] = a->valor;
        return 1;
    }
    int e = nivelV(a->esq, n - 1, v);
    int d = nivelV(a->dir, n - 1, v + e);
    return e + d;
}


// Exercício 40

int dumpAbin (ABin a, int v[], int N) {
    if (a == NULL || N == 0) return 0;
    int e = dumpAbin(a->esq, v, N);
    if (e == N) return e;
    v[e] = a->valor;
    int d = dumpAbin(a->dir, v + e + 1, N - e - 1);
    return e + 1 + d;
}





// Exercício 41

ABin somasAcA (ABin a) {
    if (a == NULL) return NULL;
    ABin nova = malloc(sizeof(struct nodo));
    nova->esq = somasAcA(a->esq);
    nova->dir = somasAcA(a->dir);
    nova->valor = a->valor;
    if (nova->esq != NULL) nova->valor += nova->esq->valor;
    if (nova->dir != NULL) nova->valor += nova->dir->valor;
    return nova;
}

// Exercício 42

int contaFolhas (ABin a) {
    if (a == NULL) return 0;
    if (a->esq == NULL && a->dir == NULL) return 1;
    return contaFolhas(a->esq) + contaFolhas(a->dir);
}

// Exercício 43

ABin cloneMirror (ABin a) {
    if (a == NULL) return NULL;
    ABin nova = malloc(sizeof(struct nodo));
    nova->valor = a->valor;
    nova->dir = cloneMirror(a->esq);
    nova->esq = cloneMirror(a->dir);
    return nova;
}

// Exercício 44

int addOrd (ABin *a, int x) {
    while (*a != NULL) {
        if ((*a)->valor == x) return 1;
        if ((*a)->valor > x) {
            a = &((*a)->esq);
        } else {
            a = &((*a)->dir);
        }
    }
    *a = malloc(sizeof(struct nodo));
    (*a)->valor = x;
    (*a)->esq = NULL;
    (*a)->dir = NULL;
    return 0;
}






// Exercício 45

int lookupAB (ABin a, int x) {
    while (a != NULL) {
        if (a->valor == x) return 1;
        if (a->valor > x) a = a->esq;
        else a = a->dir;
    }
    return 0;
}

// Exercício 46

int depthOrd (ABin a, int x) {
    int nivel = 1;
    while (a != NULL) {
        if (a->valor == x) return nivel;
        if (a->valor > x) a = a->esq;
        else a = a->dir;
        nivel++;
    }
    return -1;
}

// Exercício 47

int maiorAB (ABin a) {
    while (a->dir != NULL) {
        a = a->dir;
    }
    return a->valor;
}

// Exercício 48

void removeMaiorA (ABin *a) {
    while ((*a)->dir != NULL) {
        a = &((*a)->dir);
    }
    ABin temp = *a;
    *a = (*a)->esq;
    free(temp);
}

// Exercício 49

int quantosMaiores (ABin a, int x) {
    if (a == NULL) return 0;
    if (a->valor <= x) {
        return quantosMaiores(a->dir, x);
    } else {
        return 1 + quantosMaiores(a->dir, x) + quantosMaiores(a->esq, x);
    }
}

// Exercício 50

void listToBTree (LInt l, ABin *a) {
    if (l == NULL) return;
    ABin new = malloc(sizeof(struct nodo));
    int meio = length(l);
    LInt temp = l, prev = NULL;
    int i;
    for(i = 0; i < meio; i++){
        prev = temp;
        temp = temp->prox;
    }
    new->valor = temp->valor;
    new->esq = new->dir = NULL;
    if(prev != NULL) prev->prox = NULL;
    
    if(l!= temp) listToBTree(l, &(new->esq));
    if(temp->prox != NULL) listToBTree(temp->prox, &(new->dir));
    (*a) = new;
}

// Exrcício 51
int deProcuraAux(ABin a, int x, int maior){
    if (a == NULL) return 1;
    if((maior == 1 && a->valor <x) || (maior == 0 && a->valor > x)) return 0;
    return deProcuraAux(a->esq, x, maior) && deProcuraAux(a->dir, x, maior);
}

int deProcura (ABin a){
    if(a == NULL) return 1;
    int b = deProcuraAux(a->esq, a->valor, 0) && deProcura(a->esq);
    int c = deProcuraAux(a->dir, a->valor, 1) && deProcura(a->dir);
    return b && c;
}
