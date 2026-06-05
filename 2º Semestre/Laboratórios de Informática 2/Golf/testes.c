#include <stdio.h>
#include <stdlib.h>
#include <CUnit/CUnit.h>
#include "Jogo.h"

//
//
// Funções de Teste da Lógica
//
//

// Função que testa a validade das jogadas (valores imediatamente acima ou abaixo)
void test_jogadaValida(void) {
    carta c1 = {5, 0}; 
    carta c2 = {6, 1}; 
    carta c3 = {4, 2}; 
    carta c4 = {5, 3}; 
    CU_ASSERT(jogadaValida(c1, c2) == 1);
    CU_ASSERT(jogadaValida(c1, c3) == 1);
    CU_ASSERT(jogadaValida(c1, c4) == 0);
}

// Função que verifica se o baralho é criado corretamente com as 52 cartas
void test_criarBaralho(void) {
    jogo j;
    criarBaralho(&j);
    CU_ASSERT(j.cartas_baralho == 52);
    CU_ASSERT(j.baralho[0].num == 1);
}

// Função que testa se as cartas são baralhadas mantendo as todas no baralho
void test_baralharCartas(void) {
    jogo j;
    criarBaralho(&j);
    baralharCartas(&j);
    CU_ASSERT(j.cartas_baralho == 52);
}

// Função que testa a distribuição inicial das cartas pelas 7 pilhas
void test_distribuirCartas(void) {
    jogo j;
    criarBaralho(&j);
    distribuirCartas(&j);
    CU_ASSERT(j.cartas_baralho == 17);
    CU_ASSERT(j.quantidade_pilhas[0] == 5);
}

// Função que testa a colocação da primeira carta na pilha de descarte para iniciar
void test_descarte(void) {
    jogo j;
    criarBaralho(&j);
    distribuirCartas(&j); 
    descarte(&j);
    CU_ASSERT(j.cartas_baralho == 16);
    CU_ASSERT(j.cartas_descarte == 1);
}

// Função que testa o biscar de cartas com o baralho completo e vazio
void test_biscarCarta(void) {
    jogo j;
    j.cartas_baralho = 1; 
    j.cartas_descarte = 0;
    CU_ASSERT(biscarCarta(&j) == 1);
    CU_ASSERT(biscarCarta(&j) == 0);
}

// Função que testa mover uma carta da pilha para o descarte validando as regras
void test_moverDaPilha(void) {
    jogo j;
    j.cartas_descarte = 1;
    j.descarte[0] = (carta){5, 0}; 
    j.quantidade_pilhas[0] = 1;
    j.pilhas[0][0] = (carta){6, 1}; 
    j.quantidade_pilhas[1] = 1;
    j.pilhas[1][0] = (carta){10, 2}; 
    CU_ASSERT(moverDaPilha(&j, 1) == 1);
    CU_ASSERT(moverDaPilha(&j, 2) == 0);
}

// Função que testa se o jogo reconhece a vitória quando todas as pilhas ficam vazias
void test_verificarVitoria(void) {
    jogo j;
    for (int i = 0; i < 7; i++) j.quantidade_pilhas[i] = 5;
    CU_ASSERT(verificarVitoria(&j) == 0); 
    for (int i = 0; i < 7; i++) j.quantidade_pilhas[i] = 0;
    CU_ASSERT(verificarVitoria(&j) == 1); 
}

//
//
// Funções Auxiliares para o CUnit
//
//

// Função que adiciona todos os testes à suite
void adicionarTestes(CU_pSuite suite) {
    CU_add_test(suite, "jogadaValida", test_jogadaValida);
    CU_add_test(suite, "criarBaralho", test_criarBaralho);
    CU_add_test(suite, "baralharCartas", test_baralharCartas);
    CU_add_test(suite, "distribuirCartas", test_distribuirCartas);
    CU_add_test(suite, "descarte", test_descarte);
    CU_add_test(suite, "biscarCarta", test_biscarCarta);
    CU_add_test(suite, "moverDaPilha", test_moverDaPilha);
    CU_add_test(suite, "verificarVitoria", test_verificarVitoria);
}

// Função que cria a suit de testes
int configurarSuite(void) {
    CU_pSuite suite = CU_add_suite("Suite_Logica", NULL, NULL);
    if (NULL == suite) {
        CU_cleanup_registry();
        return 0;
    }
    adicionarTestes(suite);
    return 1;
}

// Função que verifica na lista do CUnit se um teste específico falou
int verificarSeFalhou(CU_pTest pTest) {
    int falhou = 0;
    CU_pFailureRecord pFailure = CU_get_failure_list();
    while (pFailure != NULL && falhou == 0) {
        if ((*pFailure).pTest == pTest) {
            falhou = 1;
        }
        pFailure = (*pFailure).pNext;
    }
    return falhou;
}

// Função que imprime o resultado de cada teste individualmente
void imprimirTestesDaSuite(CU_pSuite pSuite) {
    CU_pTest pTest = (*pSuite).pTest;
    while (pTest != NULL) {
        if (verificarSeFalhou(pTest) == 1) {
            printf("Funcao %s: NAO PASSOU\n", (*pTest).pName);
        } else {
            printf("Funcao %s: PASSOU\n", (*pTest).pName);
        }
        pTest = (*pTest).pNext;
    }
}

// Função que percorre as suites e inicia a impressão
void imprimirResultados(void) {
    printf("\n");
    CU_pSuite pSuite = (*CU_get_registry()).pSuite;
    while (pSuite != NULL) {
        imprimirTestesDaSuite(pSuite);
        pSuite = (*pSuite).pNext;
    }
    printf("\n");
}

//
//
// Função Main 
//
//

int main(void) {
    if (CUE_SUCCESS != CU_initialize_registry()) {
        return CU_get_error();
    }

    if (configurarSuite() == 0) {
        return CU_get_error();
    }

    CU_run_all_tests();
    imprimirResultados();

    CU_cleanup_registry();
    return CU_get_error();
}
