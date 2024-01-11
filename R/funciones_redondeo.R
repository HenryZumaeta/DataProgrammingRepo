# Funcion redondear a un múltiplo superior:
multiplo_superior <- function(m,n) {
    # Redondeamos al múltiplo superior de un número, es util
    # para determinar la cantidad de materiales, como varíllas
    # por ejemplo
    
    # Donde:
    # m = Ingrese el número a redondear
    # n = Ingrese el múltiplo
    return(n*ceiling(m/n))
}

multiplo_superior(2.7, 2.5)


# Funcion redondear(estandar):
redondear <- function(n,d) {
    # Función que redondea un número "n" a "d" decimales.
    n1 = floor(n*10^d)
    n2 = floor(n*10^(d+1))
    r = floor(n2%%10)
    if (r >= 5){
        n1 = n1 +1
    }
    return(n1/10^d)
}

redondear(2.7260875454, 3)


# Función redondar más como en excel:
redondear_mas <- function(m,n) {
    # Función que redondea hacia arriba, según los decimales requeridos
    # Donde:
    # m = Ingrese el número a redondear
    # n = Ingrese los decimales
    return (ceiling(m*10^n)/10^n)
}

print(redondear_mas(2.7260875454, 3))
