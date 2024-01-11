% PROYECTO I/PARTE 1 - BLACKJACK
clc
clear
disp("*******************************")
disp("*     Bienvenido al juego     *")
disp("*        21 BLACK JACK        *")
disp("*******************************")
disp(" ")

entrada = input("¿Estas listo para empezar el juego? \nEscribe Y para empezar el juego o cualquier tecla para salir: ",'s');
disp(" ")
if entrada == 'Y' || entrada == 'y' 
   % Selecciona al azar 2 cartas
   carta1 = randi(13,1); 
   carta2 = randi(13,1);
   suma = carta1 + carta2;
   
   % Muestra en pantalla las 2 cartas
   formato = " - Cartas del jugador: %1$d y %2$d \n - Suma de las cartas del jugador: %3$d \n";
   fprintf(formato,carta1,carta2,suma)
   disp(" ")
   
   % Condicional para seguir en el juego
   if suma < 21
      opc0 = -1;
      
     while opc0 <= 0
           opc1 = input("¿Quieres más cartas? \nEscribe Y para recibir una carta más o N para detenerte: ",'s');
           disp(" ")
           
           switch opc1;     
               case {'Y', 'y'} % Puede ser mayúscula o minúscula la entradas
                    carta3 = randi(13,1);
                    suma = suma + carta3;
                    if suma == 21
                       fprintf(" - La nueva carta del jugador: " + carta3 + "\n")
                       fprintf("Suma de cartas del jugador: " + suma + "\n")
                       disp("¡¡¡GANASTE!!!")
                       opc0 = 10
                       
                    elseif suma < 21
                        opc0 = -1; 
                        
                          if  suma > 21
                              opc0 = 10;
                              disp("salio del juego")
                              disp("Lo siento, perdiste :(")
                          end
                        fprintf(" - La nueva carta del jugador: " + carta3 + "\n")
                        fprintf(" - Suma de las cartas del jugador: " + suma + "\n")
                        
                    else
                        fprintf(" - La nueva carta del jugador: " + carta3 + "\n")  
                        fprintf("Suma de cartas del jugador: " + suma + "\n")
                        disp("Lo siento, perdiste :(")
                        opc0 = 10;
                    end
                    
               case {'N', 'n'}

                   fprintf("Suma de cartas del jugador: " + suma + "\n")
                   
                   if suma <= 21
                       disp("¡¡¡GANASTE!!!")
                       opc0 = 10;
                   end
                   
               otherwise
                   warning("Escribe la opción correcta")
                   disp(" ")
           end    
     end
     
   elseif suma == 21
       disp("¡¡¡GANASTE!!!")
       
   else
       disp("Lo siento, perdiste :(")
   end 
   
else
    disp("Gracias, vuelva cuando quiera jugar")
end

%SCRIPT DESAROLLADO POR ALUMNO: HENRY PAOLO ZUMAETA LOZANO