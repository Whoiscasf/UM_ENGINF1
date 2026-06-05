#include <stdio.h>
#include <string.h>

// 1. Defina um programa que lê (usando a função scanf) uma sequência de números inteiros terminada com o número 0 e imprime no ecrã o maior elemento da sequência.
void maior(void){
    int a,b;
    printf("Introduza o primeiro número:\n");
    scanf("%d", &a);
    b=a;
    while(a!=0){
        scanf("%d", &a);
        if(b<a) b=a;
    }
    printf("O maior elemento é: %d\n", b);
}

// 2. Defina um programa que lê (usando a função scanf) uma sequência de números inteiros terminada com o número O e imprime no ecrã a média da sequência.
void media(void){
    int a,b,c=1;
    float d=0.0;
    scanf("%d", &a);
    scanf("%d", &b);
    while(b!=0){
        a+=b;
        c++;
        scanf("%d",&b);
    }
    d = a/c;
    printf("A média é: %f",d);
}

// 3. Defina um programa que lê (usando a função scanf) uma sequência de números inteiros terminada com o número 0 e imprime no ecrã o segundo maior elemento.
void main(void){
    int valor,maior,segundo;
    scanf("%d", &valor);
    maior = valor;
    segundo = 0;
    while(valor !=0){
        if(valor>maior){
            segundo = maior;
            maior = valor;
        }
        else if(valor>segundo && maior>valor){
            segundo = valor;
        }
        scanf("%d", &valor);
    }
    printf("O segundo maior número é: %d\n", segundo);
}

// 4. Defina uma função int bitsUm (unsigned int n) que calcula o número de bits iguais a 1 usados na representação binária de um dado número n.
int bitsUm (unsigned int a){
    int total=0;
    while(a !=0){
        if (a%2 ==1 ) total++;
        a= a/2;
    }
    return total;
}

// 5. Defina uma função int trailingZ (unsigned int n) que calcula o número de bits a 0 no final da representação binária de um número (i.e., o expoente da maior potência de 2 que é divisor desse número).
int trailingZ (unsigned int n) {
    int r=0;
    while (n%2==0) {
        r += 1;
        n = n / 2;
    }
    return r;
}

// 6. Defina uma função int qDig (unsigned int n) que calcula o número de dígitos necessários para escrever o inteiro n em base decimal.
int qDig (unsigned int n) {
    int r = 0;
    while (n > 0) {
        r += 1;
        n = n / 10;
    }
    return r;
}

// 7. Apresente uma definição da função pré-definida em C char *strcat (char s1[], char s2[]) que concatena a string s2 a s1 (retornando o endereço da primeira).
char *strcat (char s1[], char s2[]) {
    int i=0, j=0;
    while (s1[i]!='\0') i++;
    while (s2[j]!='\0') {
        s2[i] = s2[j];
        i++; j++;
    }
    return s1;
}










//8. Apresente uma definição da função pré-definida em C char *strcpy (char *dest, char source[]) que copia a string source para dest, retornando o valor desta última.
char *strcpy (char *dest, char source[]) {
    int i=0;
    while (source[i]!='\0') {
        dest[i] = source[i];
        i++;
    }
    dest[i]='\0';
    return dest;
}

// 9. Apresente uma definição da função pré-definida em C int strcmp (char s1[], char s2[]) que compara (lexicograficamente) duas strings.
int *strcmp (char s1[], char s2[]) {
    int i=0;
    while (s1[i]!=0 && s2[i]!=0 && s1[i]==s2[i]) {
        i++;
        }
    return s1[i]-s2[i];
}

// 10. Apresente uma definição da função pré-definida em C char *strstr (char s1[], char s2[]) que determina a posição onde a string s2 ocorre em s1. A função deverá retornar NULL caso s2 não ocorra em s1.
char *strstr (char s1[], char s2[]) {
    for (int i=0; s1[i]!='\0'; i++) {
        int j=0;
        while (s2[j]!='\0' && s1[i+j]==s2[j]) j++;
        if (!s2[j]) return &s1[i];
        }
    return NULL;
}

void strrev (char s[]) {
    int i = 0, j = 0;
    char tam;
    while (s[j] != '\0') {
        j++;
    }
    j--;
    while (i < j) {
        tam = s[i];
        s[i] = s[j];
        s[j] = tam;
        
        i++;
        j--;
    }
}

// 12. Defina uma função void strnoV (char s[]) que retira todas as vogais de uma string.
void strnoV (char s[]) {
    int i=0,j=0;
    while (s[i]!='\0') {
        if (s[i]!='A' && s[i]!='E' && s[i]!='I' && s[i]!='O' && s[i]!='U' && s[i]!='a' && s[i]!='e' && s[i]!='i' && s[i]!='o' && s[i]!='u') {
            s[j]=s[i];
            j++;
        }
        i++;
    }
    s[j]='\0';
}

// 13. Defina uma função void truncW (char t[], int n) que dado um texto t com várias palavras (as palavras estão separadas em t por um ou mais espaços) e um inteiro n, trunca todas as palavras de forma a terem no máximo n caracteres.
void truncW (char t[], int n) {
    int i = 0; // Onde estou a LER
    int j = 0; // Onde estou a ESCREVER
    int c = 0; // Quantas letras já escrevi na palavra atual

    while (t[i] != '\0') {
        if (t[i] == ' ') {
            t[j] = t[i];
            j++;
            i++;
            c = 0;
        }
        else {
            if (c < n) {
                t[j] = t[i];
                j++;
                c++;
            }
            i++;
        }
    }
    t[j] = '\0';
}












// 14. Defina uma função char charMaisFreq (char s[]) que determina qual o caracter mais frequente numa string. A função deverá retornar 0 no caso de s ser a string vazia.
char charMaisFreq(char s[]) {
    int i, j;
    int contagem;
    int max_contagem = 0;
    char mais_freq;
    for (i = 0; s[i] != '\0'; i++) {
        contagem = 0;
        for (j = 0; s[j] != '\0'; j++) {
            if (s[i] == s[j]) {
                contagem++;
            }
        }
        if (contagem > max_contagem) {
            max_contagem = contagem;
            mais_freq = s[i];
        }
    }
    
    return mais_freq;
}
// 15. Defina uma função int iguaisConsecutivos (char s[]) que, dada uma string s calcula o comprimento da maior sub-string com caracteres iguais.
int iguaisConsecutivos (char s[]){
    int i, j, contador, max = 0;
    for (i = 0; s[i] != '\0'; i++){
        contador = 0;
        for (j = i; s[j] != '\0' && s[i] == s[j]; j++){
            contador++;
        }
        if (contador > max){
            max = contador;
        }
    }
    
    return max;
}














// 16. Defina uma função int difConsecutivos (char s[]) que, dada uma string s calcula o comprimento da maior sub-string com caracteres diferentes.
int difConsecutivos (char s[]) {
    if (s[0] == '\0') {
        return 0;
    }
    int max = 1;
    int contador = 1;
    for (int i = 1; s[i] != '\0'; i++) {
        if (s[i] != s[i - 1]) {
            contador++;
            if (contador > max) {
                max = contador;
            }
        }
        else {
            contador = 1;
        }
    }
    return max;
}

// 17. Defina uma função int maiorPrefixo (char s1[], char s2[]) que calcula o comprimento do maior prefixo comum entre as duas strings.
int maiorPrefixo (char s1 [], char s2 []) {
    int i;
    for(i = 0; s1[i] == s2[i] && s1[i] != '\0'; i++);
    return i;
}

// 18. Defina uma função int maiorSufixo (char s1[], char s2[]) que calcula o comprimento do maior sufixo comum entre as duas strings.
int maiorSufixo (char s1 [], char s2 []) {
    int n, m, contador = 0;
    for(n = 0; s1[n] != '\0'; n++);
    for(m = 0; s2[m] != '\0'; m++);
    while(n > 0 && m > 0 && s1[n-1] == s2[m-1]) {
        contador++;
        n--;
        m--;
    }
    return contador;
}










// 19. Defina a função int sufPref (char s1[], char s2[]) que calcula o tamanho do maior sufixo de s1 que é um prefixo de s2.
int sufPref (char s1[], char s2[]) {
    int i, j;
    for (i = 0; s1[i] != '\0'; i++) {
        for (j = 0; s1[i + j] == s2[j] && s1[i + j] != '\0'; j++);
        if (s1[i + j] == '\0') {
            return j;
        }
    }
    return 0;
}

// 20. Defina uma função int contaPal (char s[]) que conta as palavras de uma string. Uma palavra é uma sequência de caracteres (diferentes de espaço) terminada por um ou mais espaços.
int contaPal (char s[]) {
    int i, contador = 0;
    for (i = 0; s[i] != '\0'; i++) {
        if (s[i] != ' ' && (s[i + 1] == ' ' || s[i + 1] == '\0')) {
            contador++;
        }
    }
    return contador;
}

// 21. Defina uma função int contaVogais (char s[]) que retorna o número de vogais da string s. Não se esqueça de considerar tanto maiúsculas como minúsculas.
int contaVogais (char s[]){
    int i, contador=0;
    for(i = 0; s[i] !='\0'; i++){
        if(s[i] == 'A' || s[i] == 'E' || s[i] == 'I' || s[i] == 'O' || s[i] == 'U' ||
           s[i] == 'a' || s[i] == 'e' || s[i] == 'i' || s[i] == 'o' || s[i] == 'u')
            contador++;
    }
    return contador;
}

















// 22. Defina uma função int contida (char a[], char b[]) que testa se todos os caracteres da primeira string também aparecem na segunda.
int contida (char a[], char b[]) {
    int i, j;
    int encontrou;
    for (i = 0; a[i] != '\0'; i++) {
        encontrou = 0;
        for (j = 0; b[j] != '\0' && encontrou == 0; j++) {
            if (a[i] == b[j]) {
                encontrou = 1;
            }
        }
        if (encontrou == 0) {
            return 0;
        }
    }
    return 1;
}

// 23. Defina uma função int palindroma (char s[]) que testa se uma palavra é palíndroma, i.e., lê-se de igual forma nos dois sentidos.
int palindroma (char s[]){
    int i, b = (strlen(s) - 1);
    for (i = 0; s[i] != '\0'; i++){
        if (s[i] != s[b - i]) {
            return 0;
        }
    }
    return 1;
}

// 24. Defina uma função int remRep (char x[]) que elimina de uma string todos os caracteres que se repetem sucessivamente deixando lá apenas uma cópia. A função deverá retornar o comprimento da string resultante.
int remRep (char x[]) {
    int i;
    int escrita = 0;
    for (i = 0; x[i] != '\0'; i++) {
        if (x[i] != x[i+1]) {
            x[escrita] = x[i];
            escrita++;
        }
    }
    x[escrita] = '\0';
    return escrita;
}








// 25. Defina uma função int limpaEspacos (char t[]) que elimina repetições sucessivas de espaços por um único espaço. A função deve retornar o comprimento da string resultante.
int limpaEspacos (char t[]) {
    int i;
    int escrita = 0;
    for (i = 0; t[i] != '\0'; i++) {
        if (t[i] != ' ' || t[i+1] != ' ') {
            t[escrita] = t[i];
            escrita++;
        }
    }
    t[escrita] = '\0';
    return escrita;
}

// 26. Defina uma função void insere (int v[], int N, int x) que insere um elemento (x) num vetor ordenado. Assuma que as N primeiras posições do vetor estão ordenadas e que por isso, após a inserção, o vetor terá as primeiras N+1 posições ordenadas.
void insere (int v[], int N, int x) {
    int i = N - 1;
    while (i >= 0 && v[i] > x) {
        v[i + 1] = v[i];
        i--;
    }
    v[i + 1] = x;
}

// 27. Defina uma função void merge (int r [], int a[], int b [], int na, int nb) que, dados vetores ordenados a (com na elementos) e b (com nb elementos), preenche o vetor r (com na+nb elementos) com os elementos de a e b ordenados.
void merge (int r[], int a[], int b[], int na, int nb) {
    int i = 0, j = 0;
    
    while (i + j < na + nb) {
        // Se ainda houver elementos em 'a'
        // E (ou 'b' já acabou OU o elemento de 'a' é menor)
        if (i < na && (j == nb || a[i] < b[j])) {
            r[i + j] = a[i];
            i++;
        }
        else {
            // Se 'a' acabou ou o elemento de 'b' for menor
            r[i + j] = b[j];
            j++;
        }
    }
}








// 28. Defina uma função int crescente (int a [], int i, int j) que testa se os elementos do vetor a, entre as posições i e j (inclusive) estão ordenados por ordem crescente. A função deve retornar 1 ou 0 consoante o vetor esteja ou não ordenado.
int crescente (int a[], int i, int j) {
    int k, cresc = 1;
    for(k = i; k < j && cresc; k++)
        if(a[k + 1] < a[k])
            cresc = 0;
    return cresc;
}



// 29. Defina uma função int retiraNeg (int v [], int N) que retira os números negativos de um vetor com N inteiros. A função deve retornar o número de elementos que não foram retirados.
int retiraNeg (int v[], int N) {
    int i;
    int escrita = 0;
    for (i = 0; i < N; i++) {
        if (v[i] >= 0) {
            v[escrita] = v[i];
            escrita++;
        }
    }
    return escrita;
}

// 30. Defina uma função int menosFreq (int v[], int N) que recebe um vetor v com N elementos ordenado por ordem crescente e retorna o menos frequente dos elementos do vetor. Se houver mais do que um elemento nessas condições deve retornar o que começa por aparecer no índice mais baixo.
int menosFreq (int v[], int N) {
    int i, freq = 1, minFreq = N, ans = v[0];
    for(i = 1; i < N; i++) {
        if(v[i] == v[i - 1]) freq++;
        else {
            if(freq < minFreq) {
                minFreq = freq;
                ans = v[i - 1];
            }
            freq = 1;
        }
    }
    if(freq < minFreq) {
        minFreq = freq;
        ans = v[i - 1];
    }
    return ans;
}

// 31. Defina uma função int maisFreq (int v[], int N) que recebe um vetor v com N elementos ordenado por ordem crescente e retorna o mais frequente dos elementos do vetor. Se houver mais do que um elemento nessas condições deve retornar o que começa por aparecer no índice mais baixo.
int maisFreq (int v[], int N) {
    int i, freq = 1, maxFreq = 0, ans = v[0];
    for(i = 1; i < N; i++) {
        if(v[i] == v[i - 1]) freq++;
        else {
            if(freq > maxFreq) {
                maxFreq = freq;
                ans = v[i - 1];
            }
            freq = 1;
        }
    }
    if(freq > maxFreq) {
        maxFreq = freq;
        ans = v[i - 1];
    }
    return ans;
}
// 32. Defina uma função int maxCresc (int v [], int N) que calcula o comprimento da maior sequência crescente de elementos consecutivos num vetor v com N elementos.
int maxCresc (int v[], int N) {
    int max = 1;
    int contador = 1;
    for (int i = 1; i < N; i++) {
        if (v[i] >= v[i - 1]) {
            contador++;
            if (contador > max) {
                max = contador;
            }
        }
        else {
            contador = 1;
        }
    }

    return max;
}














// 33. Defina uma função int elimRep (int v[], int n) que recebe um vetor v com n inteiros e elimina as repetições. A função deverá retornar o número de elementos do vetor resultante.
int elimRep (int v[], int n) {
    int i, j;
    int escrita = 0;
    int repetido;
    for (i = 0; i < n; i++) {
        repetido = 0;
        for (j = 0; j < escrita && repetido == 0; j++) {
            if (v[i] == v[j]) {
                repetido = 1;
            }
        }
        if (repetido == 0) {
            v[escrita] = v[i];
            escrita++;
        }
    }
    return escrita;
}
// 34. Defina uma função int elimRepOrd (int v[], int n) que recebe um vetor v com n inteiros ordenado por ordem crescente e elimina as repetições. A função deverá retornar o número de elementos do vetor resultante.
int elimRepOrd (int v[], int n) {
    int i;
    int escrita = 1;
    for (i = 1; i < n; i++) {
        if (v[i] != v[i - 1]) {
            v[escrita] = v[i];
            escrita++;
        }
    }
    return escrita;
}
// 35. Defina uma função int comunsOrd (int a[], int na, int b[], int nb) que calcula quantos elementos os vetores a (com na elementos) e b (com nb elementos) têm em comum. Assuma que os vetores a e b estão ordenados por ordem crescente.
int comunsOrd (int a[], int na, int b[], int nb) {
    int i = 0, j = 0, ans = 0;
    while(i < na && j < nb) {
        if(a[i] == b[j]) {
            ans++;
            i++;
            j++;
        }
        else if(a[i] > b[j]) j++;
        else i++;
    }
    return ans;
}

// 36. Defina uma função int comuns (int a[], int na, int b[], int nb) que calcula quantos elementos os vetores a (com na elementos) e b (com nb elementos) têm em comum. Assuma que os vetores a e b não estão ordenados e defina a função sem alterar os vetores.
int comuns (int a[], int na, int b[], int nb) {
    int i, j;
    int contador = 0;
    int repetido, encontrado;
    for (i = 0; i < na; i++) {
        repetido = 0;
        for (j = 0; j < i && repetido == 0; j++) {
            if (a[i] == a[j]) {
                repetido = 1;
            }
        }
        if (repetido == 0) {
            encontrado = 0;
            for (j = 0; j < nb && encontrado == 0; j++) {
                if (a[i] == b[j]) {
                    encontrado = 1;
                }
            }
            if (encontrado == 1) {
                contador++;
            }
        }
    }
    
    return contador;
}

// 37. Defina uma função int minInd (int v[], int n) que, dado um vetor v com n inteiros, retorna o índice do menor elemento do vetor.
int minInd (int v[], int n) {
    int i;
    int imin = 0;
    for (i = 1; i < n; i++) {
        if (v[i] < v[imin]) {
            imin = i;
        }
    }
    return imin;
}

// 38. Defina uma função void somasAc (int v[], int Ac[], int N) que preenche o vetor Ac com as somas acumuladas do vetor v. Por exemplo, a posição Ac[3] deve ser calculada como v[0] + v[1] + v[2] + v[3].
void somasAc (int v[], int Ac [], int N) {
    int i, j, acc = 0;
    for(i = 0; i < N; i++) {
        acc += v[i];
        Ac[i] = acc;
    }
}
