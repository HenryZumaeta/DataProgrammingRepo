% PROYECTO I/PARTE 3 - PROMEDIO PONDERADO
clc
clear
fprintf('*** BIENVENIDO A LA CALCULADORA DE PROMEDIOS PONDERADOS ***\n- DATOS DE ENTRADA:\n')
cantPrac = input("Ingrese la cantidad de Prácticas: ");
cantExam = input("Ingrese la cantidad de Exámenes: ");
pesoPrac = input("Ingresa el peso de la práctica: ");
pesoExam = input("Ingresa el peso de examen: ");
pesos = [pesoPrac pesoExam];

formato = '- Peso de la práctica es: %.1f\n- Peso de exámenes es:  %.1f\n';

sumPrac = 0;
sumExam = 0;

for i = 1:cantPrac
    notaPrac = input("Ingrese la nota de la práctica " + i + ": ");
    sumPrac = sumPrac + notaPrac*pesoPrac;
end

for i = 1:cantExam
    notaExam = input("Ingrese la nota del examen " + i + ": ");
    sumExam = sumExam + notaExam*pesoExam;
end
disp("==========================")
fprintf(formato,pesos)

promPond = (sumExam + sumPrac)/(cantExam*pesoExam + cantPrac*pesoPrac);

disp("El promedio Ponderado es = " + round(promPond))

if round(promPond)>=11
   disp("¡¡¡Felicitaciones pasaste la materia!!!")
else 
    disp("Lo siento, no pasaste la materia. :(")
end
    
disp("==========================")

%SCRIPT DESAROLLADO POR ALUMNO: HENRY PAOLO ZUMAETA LOZANO
