/* Juego en C, Ansi-Style */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main(void)
{
puts("Hola! Trataré de adivinar un número.");
puts("Piensa un número entre 1 y 10.");
sleep(5);
puts("Ahora multiplícalo por 9.");
sleep(5);
puts("Si el número tiene 2 dígitos, súmalos entre si: Ej. 36 -> 3+6=9. Si tu número tiene un solo dígito, súmale 0.");
sleep(5);
puts("Al número resultante súmale 4.");
sleep(8);
puts("Muy bien. El resultado es 13 :)");
return(EXIT_SUCCESS);
}



