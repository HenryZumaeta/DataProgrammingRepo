function [Y] = matrizMaxNoCero(fila,columna)
% La función hace que todos los elementos de una matriz se transformen en 
% ceros, excepto el elemento mayor de cada columna.
X = randi(100,fila,columna); % Datos aleatorios
Ymax = max(X);
M = ~(X./Ymax<1).*X;
fprintf("\nMatriz de Entrada es:\n");
disp(X);
fprintf("\nMaximo de cada columnas es:\n");
disp(Ymax);
fprintf("\nMatriz de Salida es:\n");
disp(M);
end