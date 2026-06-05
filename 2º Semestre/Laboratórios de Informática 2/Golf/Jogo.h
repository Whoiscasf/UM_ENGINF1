// Variável "carta"
typedef struct {
    int num;        // número (1 a 13)
    int naipe;      // naipe (0 a 3)
} carta;

// Variáveis "pilhas";"cartas_baralho";"cartas_descarte"
typedef struct {
    carta pilhas[7][5];
    int quantidade_pilhas[7];
    carta baralho[52];
    int cartas_baralho;
    carta descarte[52];
    int cartas_descarte;
} jogo;

// Interface gráfica do jogo
void imprimirCarta(carta c);
void mostrarCabecalho(void);
void mostrarMesa(jogo *j);
void mostrarPilhas(jogo *j);
void vitoria(void);
int pedirOpcao(void);
void executarBiscar(jogo *j);
void executarMoverPilha(jogo *j, int opcao);
int processarJogada(jogo *j);

// Lógica de jogo
int jogadaValida(carta c, carta d);
void criarBaralho(jogo *j);
void baralharCartas(jogo *j);
void distribuirCartas(jogo *j);
void descarte(jogo *j);
void comecarJogo(jogo *j);
int biscarCarta(jogo *j);
int moverDaPilha(jogo *j, int opcao);
int verificarVitoria(jogo *j);
