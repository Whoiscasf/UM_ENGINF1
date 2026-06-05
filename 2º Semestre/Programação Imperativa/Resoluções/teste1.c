//
//  teste.c
//  Desktop
//
//  Created by Carlos Alexandre  Silva Ferreira  on 04.03.2026.
//
#include <stdio.h>
// Exercício 1

 int main(void){
     int inicio,fim,i,j,primo,contador=0;
     scanf("%d", &inicio);
     scanf("%d", &fim);
     
     for(i=inicio; i<=fim;i++){
         if(i<=1)
             primo = 0;
         else {
             primo = 1;
             for(j=2; j*j <= i && primo == 1; j++){
                 if(i % j == 0)
                     primo = 0;
             }
         }
         if (primo == 1)
             contador++;
     }
     
     printf("O número de primos é: %d\n",contador);
     return 0;
 }
/*
//Exercício 2
int main(void){
    int numero,i,j,primo,mdc=0;
    scanf("%d", &numero);
    
    for(i=numero; i>=2;i--){
        if(numero%i == 0){
            primo = 1;
            for(j=2; j*j<=i;j++){
                if(i%j == 0)
                    primo = 0;
            }
        }
        if(primo == 1 && mdc == 0)
            mdc = i;
        
    }
    if (mdc !=0)
        printf("O mdc primo é: %d", mdc);
    else
        printf("Não tem mdc primo");
    return 0;
}
 */
