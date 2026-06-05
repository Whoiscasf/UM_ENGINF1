#include <stdio.h>
#include

int fizzbuzz(int n ) {
    int fb = 0;
    int c = 0;
    for (int i = 0; i< n; i++) {
        if (fizz(i) && buzz(i)) {
            if (fb == 1) {
                printf("FizzBuzz\n");
            }
            else {
                printf("FizzBuzz\n");
                fb = 1;
            }
        }
        else if (fizz(i)) {
            printf("Fizz\n");
        }
        else if (buzz(i)) {
            if (fb == 1) {
                printf("Buzz\n");
                return c;
            } else {
                printf("Buzz\n");
            }
        }
        else {
            if (fb == 1){
                printf("%d\n", i);
                c++;
            }
            else printf("%d\n", i);
        }
    }
    return -1;
}

void rodaEsq(int a[], int N, int r) {
    int temp[r];
    for (int i = 0; i < r; i++) {
        temp[i] = a[i];
    }
    for (int i = r; i < N; i++) {
        a[i - r] = a[i];
    }
    for (int i = 0; i < r; i++) {
        a[N - r + i] = temp[i];
    }
}

typedef struct lint_nodo {
    int valor;
    struct lint_nodo *prox;
} *LInt;

int delete(int n, LInt *l) {
    if (*l == NULL || n < 0) {
        return 1;
    }
    if (n == 0) {
        LInt temp = *l;
        *l = (*l)->prox;
        free(temp);
        return 0;
    }
    return delete(n - 1, &((*l)->prox));
}

int maxCresc(LInt l) {
    if (l == NULL) {
        return 0;
    }
    int max = 1;
    int atual = 1;
    while (l->prox != NULL) {
        if (l->valor < l->prox->valor) {
            atual++;
        }
        else {
            if (atual > max) {
                max = atual;
            }
            atual = 1;
        }
        l = l->prox;
    }
    if (atual > max) {
        max = atual;
    }
    return max;
}

typedef struct abin_nodo {
    int valor;
    struct abin_nodo *esq, *dir;
} *ABin;

ABin folhasEsq(ABin a) {
    if (a == NULL) return NULL;
    else if (a->esq == NULL && a->dir == NULL) return a;
    else if (a->esq != NULL) {
        return folhasEsq (a->esq);
    }
    return folhasEsq(a->dir);
}

int parentesisOk(char exp[]) {
    char pilha[1000];
    int topo = 0;
    for (int i = 0; exp[i] != '\0'; i++) {
        if (exp[i] == '(' || exp[i] == '[' || exp[i] == '{') {
            pilha[topo] = exp[i];
            topo++;
        }
        else if (exp[i] == ')' || exp[i] == ']' || exp[i] == '}') {
            if (topo == 0) {
                return 0;
            }
            topo--;
            char ultimoAberto = pilha[topo];
            if (exp[i] == ')' && ultimoAberto != '(') return 0;
            if (exp[i] == ']' && ultimoAberto != '[') return 0;
            if (exp[i] == '}' && ultimoAberto != '{') return 0;
        }
    }
    if (topo == 0) {
        return 1;
    } else {
        return 0;
    }
}











