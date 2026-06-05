typedef struct {
    int num;        // número (1 a 13)
    int naipe;      // naipe (0 a 3)
} carta;

typedef struct {
    carta pilhas[10][52];
    int quantidade_pilhas[10];
    carta baralho[52];
    int pilhas_concluidas;
} jogo;

// Lógica de jogo (logica.c)
void criarBaralho(jogo *j);
void baralharCartas(jogo *j);
void distribuirCartas(jogo *j);
void comecarJogo(jogo *j);
int limitesValidos(jogo *j, int o, int d, int q);
int sequenciaValida(jogo *j, int origem, int qtd);
int destinoValido(jogo *j, carta movida, int destino);
int validarJogadaTotal(jogo *j, int o, int d, int q);
void moverCartas(jogo *j, int orig, int dest, int qtd);
void tentarRemoverSequencia(jogo *j, int col);
int executarMovimento(jogo *j, int o, int d, int q);

// Interface gráfica (interface.c)
void imprimirCarta(carta c);
void mostrarCabecalho(void);
void instrucoes(void);
void mostrarMesa(jogo *j);
int obterMaxLinhas(jogo *j);
void mostrarPilhas(jogo *j);
void vitoria(void);
void imprimirErro(int erro);
void imprimirResultado(int v);
int processarJogada(jogo *j);
