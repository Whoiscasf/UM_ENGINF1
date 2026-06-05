#include <stdlib.h>
#include <time.h>
#include "Jogo.h"

//
//
// Função Main
//
//

int main(void) {
    srand((unsigned int)time(NULL));
    jogo Jogo_golf;
    comecarJogo(&Jogo_golf);
    int aJogar = 1;
    while (aJogar == 1) {
        mostrarCabecalho();
        mostrarMesa(&Jogo_golf);
        mostrarPilhas(&Jogo_golf);

        if (verificarVitoria(&Jogo_golf) == 1) {
            vitoria();
            aJogar = 0; // Termina o ciclo
        } else {
            // Chama a interface e atualiza o estado do jogo
            aJogar = processarJogada(&Jogo_golf);
        }
    }
    return 0;
}
