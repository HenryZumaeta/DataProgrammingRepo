% PROYECTO II/PARTE 2 - TABLAS DE INFORMACION DE LOS PLANETAS
clc
clear
disp("****************************************************")
disp("*                    BIENVENIDO                    *")
disp("*        Tablas informativas de los Planetas       *")
disp("****************************************************")
disp(" ")

% FATOS:
Planetas = ["MERCURIO"; "VENUS"; "TIERRA"; "MARTE" ;"JUPITER"; "SATURNO"; "URANO"; "NEPTUNO"];
Masas = [3.3E+23 ;4.87E+24 ;5.97E+24 ;6.42E+23; 1.9E+27; 5.68E+26 ;8.68E+25; 1.02E+26];
Distancia = [57909227 ;108209475; 149598262; 227943824; 778340821; 778340821; 2870658186; 4498396441];
Tipo = ["Rocoso"; "Rocoso"; "Rocoso"; "Rocoso"; "Gaseoso"; "Gaseoso"; "Gaseoso"; "Gaseoso"];
Lunas = [0; 0; 1; 2; 79; 82; 27; 14];
disp("Digite el número correspondiente a las tablas que quiere ver:")
disp("1. Tabla de planetas rocosos")
disp("2. Tabla de planetas gaseosos")
disp("3. Tabla de todos los planetas")
disp(" ")

tabla = table(Masas,Distancia,Tipo,Lunas,'RowNames',Planetas);
planRocosos = tabla(1:4,:);
planGaseosos = tabla(5:8,:);
opc0 = -1;
while opc0 < 0
    
    opc = input("Ingrese la opción: ");
    disp(" ")
    switch opc
    case 1
        disp("Tabla de Planetas Rocosos: ")
        disp(planRocosos)
        break
    case 2
        disp("Tabla de Planetas Gaseosos: ")
        disp(planGaseosos)
        break
    case 3
        disp("Tabla de todos los Planetas: ")
        disp(tabla)
        break
    otherwise
        opc0 = -1;
        warning("Escribe la opción correcta")
        disp(" ")         
end
end

%SCRIPT DESAROLLADO POR ALUMNO: HENRY PAOLO ZUMAETA LOZANO