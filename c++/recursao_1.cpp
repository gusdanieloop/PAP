#include <bits/stdc++.h>

using namespace std;

int somaAteN(int n)
{
    if(n == 1)
        return 1;
    return n + somaAteN(n-1);
}

int minimo(int lista[], int tamanho)
{
    if(tamanho == 1)
        return min(lista[tamanho], lista[tamanho - 1]);
    return min(lista[tamanho - 1], minimo(lista, tamanho - 1));
}


int maximo(int lista[], int tamanho)
{
    if(tamanho == 1)
        return max(lista[tamanho - 1], lista[tamanho]);
    return max(lista[tamanho - 1], maximo(lista, tamanho - 1));
}

bool palindromo(char lista[], int tamanho)
{
    if(tamanho == 1)
    {
        return true;
    }
        
    if(tamanho == 2)
    {
        if(lista[0] == lista[1])
        {
            return true;
        }
        else
        {
            return false;
        }
    }
    if(lista[tamanho - 1] != lista[0])
    {
        return false;
    }
    /*char palavranova[tamanho - 2 ];
    for(int i = 1; i < tamanho -1; ++i)
    {
        palavranova[i-1] = lista[i];
    }*/
    return palindromo(lista+1, tamanho - 1);
}



int main()
{
    char testando[3] = {'o','v','o'};
    char testando3[4] = {'c','a','s','a'};
    char testando2[8] = {'a','b','c','d','d','c','b','a'};
    cout << palindromo(testando, 3) << endl;
    cout << palindromo(testando2, 8) << endl;
    cout << palindromo(testando3, 4) << endl;
    int teste = 5;
    int teste2[5] = {11,21,2,50,49};
    cout << minimo(teste2, 5) << endl;
    cout << maximo(teste2, 5) << endl;
    cout << somaAteN(teste) << endl;
    return 0;
}