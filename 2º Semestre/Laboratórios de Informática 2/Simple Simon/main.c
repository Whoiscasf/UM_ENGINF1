#include <stdlib.h>
#include <time.h>
#include "Jogo.h"

int main(void) {
    srand((unsigned int)time(NULL));
    jogo Jogo_simon;
    comecarJogo(&Jogo_simon);

    int aJogar = 1;
    while (aJogar == 1) {
        mostrarCabecalho();
        instrucoes();
        mostrarMesa(&Jogo_simon);
        mostrarPilhas(&Jogo_simon);
        
        if (Jogo_simon.pilhas_concluidas == 4) {
            vitoria();
            aJogar = 0;
        } else {
            aJogar = processarJogada(&Jogo_simon);
        }
    }
    return 0;
}
