% PROYECTO I/PARTE 2 - CALCULADORA BÁSICA
clc
clear
opc1 = 0;
    
while opc1 <= 0
    
    while 1

    fprintf('*** BIENVENIDO A LA CALCULADORA BÁSICA DE 2 NÚMEROS ***\n- OPERADORES:\n1. +\n2. -\n3. *\n4. /\n')
    disp(" ")
    opc1 = input("Digite el símbolo: ",'s');
    disp(" ")

        switch opc1;
            case '+'
                fprintf("*Elegiste la SUMA* \n",'s')
                disp(" ")
                num1 = input("Ingresa el número 01: ");
                num2 = input("Ingresa el número 02: ");
                suma = num1 + num2;
                disp("========================")
                formato = "La suma de %1$g más %2$g es: %3$g \n";
                fprintf(formato, num1, num2, suma)
                break

            case '-'
                fprintf("*Elegiste la RESTA* \n",'s')
                disp(" ")
                num1 = input("Ingresa el número 01: ");
                num2 = input("Ingresa el número 02: ");
                resta = num1 - num2;
                disp("========================")
                formato = "La resta de %1$g menos %2$g es: %3$g \n";
                fprintf(formato, num1, num2, resta)
                break

            case '*'
                fprintf("*Elegiste la MULTIPLICACIÓN* \n",'s')
                disp(" ")
                num1 = input("Ingresa el número 01: ");
                num2 = input("Ingresa el número 02: ");
                multiplicacion = num1*num2;
                disp("========================")
                formato = "La multiplicación de %1$g por %2$g es: %3$g \n";
                fprintf(formato, num1, num2, multiplicacion)
                break

            case '/'
                fprintf("*Elegiste la DIVISIÓN* \n",'s')
                disp(" ")
                num1 = input("Ingresa el número 01(Dividendo): ");
                num2 = input("Ingresa el número 02(Divisor): ");
                division = num1/num2;
                disp("========================")
                formato = "La división de %1$g entre %2$g es: %3$g \n";
                fprintf(formato, num1, num2, division)
                break

            otherwise
                warning('Símbolo incorrecto, digite el símbolo correcto')
        end
    end
disp(" ")
    opc2 = input("¿Desea realizar otra operación? \n- Digite 1 si desea continuar. \n- Digite cualquier tecla para salir. \n",'s');
    switch opc2;
        case "1"
            opc1 = -1;
        otherwise
            disp("=====================================")
            disp("=  GRACIAS POR USAR LA CALCULADORA  =")
            disp("=====================================")
    end     
end

%SCRIPT DESAROLLADO POR ALUMNO: HENRY PAOLO ZUMAETA LOZANO



