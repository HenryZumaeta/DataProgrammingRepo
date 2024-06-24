% PROYECTO II/PARTE 1 - OPERACIONES BÁSICAS DE DOS POLINÓMIOS DE CUALQUIER
% ORDEN
clc
clear
disp("****************************************************************")
disp("*                          BIENVENIDO                          *")
disp("*   Operaciones básicas de dos polinomios de cualquier orden   *")
disp("****************************************************************")
disp(" ")
disp("Los polinomios son de la forma:")
disp("P(X) = (Kn)*X^n + [K(n-1)]*X^(n-1) +...+ (K2)*X^2 + (K1)*X + K0")
disp(" ")
n = input("Ingresa el grado del primer polinomio: ");
px = leerlista(n);
disp(" ")
m = input("Ingresa el grado del segundo polinomio: ");
qx = leerlista(m);
disp(" ")
pxinver1 = invertirlista(px);   %invertimos la lista para poder multiplicar
                                % y dividir sin problemas, porque entrando
                                %al bucle crea ceros que dificultan las 
                                %operaciones de Multiplicación y División.
qxinver1 = invertirlista(qx);
mult = conv(pxinver1,qxinver1);
[coc,res] = deconv(pxinver1,qxinver1);

if n~=m
    if n>m
    for (ind1=m+2:n+1)
        qx(ind1)=0;
    end
    else
        for (ind2=n+2:m+1)
        px(ind2)=0;
        end 
    end
end
pxinver2 = invertirlista(px);   % Invertimos lista salida del bucle, para 
                                % poder Sumar y Restar de manera ordenada
                                % los polinomios.
qxinver2 = invertirlista(qx);
disp("Primer polinomio: ")
disp(pxinver2)
disp("Segundo polinomio: ")
disp(qxinver2)
disp("SALIDA DE OPERACIONES BÁSICAS LOS DOS POLINOMIOS")
disp("1. Suma:")
disp(pxinver2+qxinver2)
disp("2. Resta:")
disp(pxinver2-qxinver2)
disp("3. Multiplicación:")
disp(mult)
disp("4. División:")
disp("Cociente:")
disp(coc)
disp("Residuo")
disp(res)

% FUNCIONES LOCALES
function [X] = leerlista(n)
%Función para ingresar datos de un vector desde el teclado.
for (i=n:-1:0)
    fprintf("Ingresa el coeficiente (K"+i+"): ");
    X(i+1) = input('');
    
end
end

function [X] = invertirlista(Y)
%Función que invierte el orden de los datos de Y.
n = length(Y);
q = n;
for (i=1:n)
    X(i) = Y(q);
    q = q-1;
end
end
%SCRIPT DESAROLLADO POR ALUMNO: HENRY PAOLO ZUMAETA LOZANO

eps(1e10)

