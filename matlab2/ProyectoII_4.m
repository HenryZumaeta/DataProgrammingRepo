% PROYECTO II/PARTE 4 - LOS ELEMENTOS DE UNA MATRIZ SE TRANSFORME EN CERO,
% EXCEPTO EL ELEMENTO MAYOR DE CADA COLUMNA

%% COMO SCRIPT:
clc
clear
disp("******************")
disp("*   BIENVENIDO   *")
disp("******************")
disp(" ")
disp("Todos los elementos de una matriz se transformarán en ceros,")
disp("excepto el elemento mayor de cada columna.")
disp(" ")
fila = input("Ingrese número de filas: ");
columna = input("Ingrese número de columnas: ");
X = randi(100,fila,columna); % Datos aleatorios
Ymax = max(X);
M = ~(X./Ymax<1).*X;
fprintf("\nMatriz de Entrada es:\n");
disp(X);
fprintf("\nMaximo de cada columnas es:\n");
disp(Ymax);
fprintf("\nMatriz de Salida es:\n");
disp(M);


%% COMO FUNCION:
% function [Y] = matrizMaxNoCero(fila,columna)
% % La función hace que todos los elementos de una matriz se transformen en 
% % ceros, excepto el elemento mayor de cada columna.
% X = randi(100,fila,columna); % Datos aleatorios
% Ymax = max(X);
% M = ~(X./Ymax<1).*X;
% fprintf("\nMatriz de Entrada es:\n");
% disp(X);
% fprintf("\nMaximo de cada columnas es:\n");
% disp(Ymax);
% fprintf("\nMatriz de Salida es:\n");
% disp(M);
% end

%SCRIPT DESAROLLADO POR ALUMNO: HENRY PAOLO ZUMAETA LOZANO