% PROYECTO II/PARTE 3 - GRÁFICAS EN CINEMÁTICA (ACELERACION, VELOCIDAD Y
% POSICIÓN)
clc
clear
close
disp("****************************************************")
disp("*                    BIENVENIDO                    *")
disp("*              Gráficas en Cinemática              *")
disp("****************************************************")
disp(" ")
aceleracion = input("Ingrese la aceleración, m/s2: ");
vo = input("Ingrese la velocidad inicial, m/s: ");
po= input("Ingrese la posición inicial, m: ");
tf=input("Ingrese el tiempo, s: ");
velocidad = polyint(aceleracion,vo);
posicion = polyint(velocidad,po);
t=0:tf;
disp(" ")
disp("SALIDA DE POLINOMIOS")
disp("Aceleración:")
disp(aceleracion);
disp("Velocidad:")
disp(velocidad);
disp("Posición:")
disp(posicion);
acel = polyval(aceleracion,t);
vel = velocidad(1)*t+velocidad(2);
pos = posicion(1)*t.^2+posicion(2).*t+posicion(3);
var = [aceleracion,vo,po,vel,pos];%Lista de variables
minimo = min(var);% Determina el mínimo de la lista de variables
maximo = max(var);% Determina el máximo de la lista de variables
rango = maximo - minimo; % rango entre el mímino y máximo
ymax = maximo + 0.2*rango; % Para determinar el límite máximo de la gráfica
ymin = minimo - 0.2*rango; % Para determinar el límite mínimo de la gráfica
hold on
grid on
plot(t,vel,"m-","LineWidth",1.25)
plot(t,pos,"b-","LineWidth",1.25)
plot(t,acel,"c-","LineWidth",1.25)
xlim([0 tf])
ylim([ymin ymax])
title("GRÁFICAS EN CINEMÁTICA USANDO MATLAB","(Aceleración, Velocidad y Posición)",...
    'fontname','agency fb','FontSize',12,'Color','k')
xlabel('Tiempo, s','fontname','agency fb','FontSize',10,'FontWeight',...
    'bold','Color','k');
ylabel('Aceleración(m/s2), Velocidad(m/s) y Posición(m)','fontname',...
    'agency fb','FontSize',10,'FontWeight','bold','Color','k');
leyenda = legend();
leyenda.Color = 'w';
leyenda.FontName ='agency fb';
leyenda.String{1} ='Velocidad';
leyenda.String{2} ='Posición';
leyenda.String{3} ='Aceleración';


%SCRIPT DESAROLLADO POR ALUMNO: HENRY PAOLO ZUMAETA LOZANO