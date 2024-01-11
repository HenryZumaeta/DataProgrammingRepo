% PROYECTO I/PARTE 4 10 ALUMNOS Y 5 PRÁCTICAS
clc
clear
disp("********************************************")
disp("*                BIENVENIDO                *")
disp("*    Matriz de 10 alumnos y 5 prácticas    *")
disp("********************************************")
disp(" ")
alumnos = 10; % Numero de filas
practicas = 5; % Numero de columnas
NOTAS = randi(20,alumnos,practicas); % Matriz de notas de 5 practicas de los 10 alumnos
listAlum = ["Jose"; "Juan"; "Carlos"; "Miguel"; "Luis"; "Rosa"; "Videlmo"; "Susana"; "Manuel"; "Alex"];

AlumnoPromedio = zeros(alumnos,2);

Matriz = table(NOTAS, 'rowNames',listAlum )

disp("--------------------------------")
disp("LISTA DE ALUMNOS Y SUS PROMEDIOS")
disp("--------------------------------")


for alumno = 1:alumnos
    suma = 0;
    for practica = 1:practicas
        suma = suma + NOTAS(alumno,practica);
        
    end
    promedio = suma/practicas;
    nombre = listAlum(alumno);
    AlumnoPromedio(alumno,1) = alumno;
    AlumnoPromedio(alumno,2) = promedio;
  
    formato = "%1$s: %2$g \n";
    fprintf(formato, nombre, promedio)
    
end

indPromMayor = 1;
promedioMayor = 0;
promediosMayores = zeros(alumnos,1);

disp(" ")
disp("----------------------------------------")
disp("LISTA DE ALUMNOS APROBADOS Y SU PROMEDIO")
disp("----------------------------------------")
for alumno = 1:alumnos
    if AlumnoPromedio(alumno,2) >= 11
    formato = "%1$s: %2$g \n";
    fprintf(formato, listAlum(AlumnoPromedio(alumno,1)), AlumnoPromedio(alumno,2)) 
    end
    
    if promedioMayor < AlumnoPromedio(alumno,2)
       promedioMayor = AlumnoPromedio(alumno,2);
       indPromMayor = AlumnoPromedio(alumno,1);
       
    end 
    
end
disp(" ")
disp("-------------------------")
disp("ALUMNO COM MAYOR PROMEDIO")
disp("-------------------------")
for alumno = 1:alumnos
    
    if promedioMayor == AlumnoPromedio(alumno,2)
       formato = "%1$s: %2$g \n";
       fprintf(formato, listAlum(AlumnoPromedio(alumno,1)), promedioMayor) 
  
    end 
end

%SCRIPT DESAROLLADO POR ALUMNO: HENRY PAOLO ZUMAETA LOZANO
