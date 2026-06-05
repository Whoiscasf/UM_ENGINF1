#include <stdio.h>
#include <stdlib.h>

typedef struct slist {
    int valor;
    struct slist * prox;
} * LInt;

LInt newLInt(int x, LInt xs) {
    LInt r = malloc(sizeof(struct slist));
    if (r != NULL) {
        r->valor = x; r->prox=xs;
    }
    return r;
}
// Exercício 1

void initStack (Stack *s) {
    *s = NULL;
}

int SisEmpty (Stack s) {
    return (s == NULL);
}

int push (Stack *s, int x) {
    int erro = 0;
    LInt a = newLInt(x, *s);
    if (a == NULL) return 1;
    *s = a;
    return 0;
}

int pop(Stack *s, int *x) {
    if (*s == NULL) return 1;
    *x = (*s)->valor;
    LInt temp = (*s)->prox;
    free(*s);
    *s = temp;
    return 0;
}

int top(Stack s, int *x) {
    if ( s == NULL) return 1;
    *x = s->valor;
    return 0;
}

// Exercíco 2

typedef struct {
    LInt inicio,fim;
} Queue;

void initQueue (Queue *q) {
    q->inicio = NULL;
    q->fim = NULL;
}

int QisEmpty (Queue q) {
    return (q.inicio == NULL)
}

int enqueue (Queue *q, int *x) {
    LInt *a = malloc(sizeof(struct Queue));
    a.valor = &x;
    if (QisEmpty (q) == 1) {
        (*q)->inicio = *a;
        (*q)->fim = *a;
    }
}

int dequeue (Queue *q, int *x) {
    if (q->inicio == NULL) return 1;
    LInt temp = q->inicio;
    *x = temp->valor;
    q->inicio = temp->prox;
    if (q->inicio == NULL) {
        q->fim = NULL;
    }
    free(temp);
    return 0;
}

int front (Queue q, int *x) {
    if (q.inicio == NULL) return 1;
    *x = q.inicio->valor;
    return 0;
}
