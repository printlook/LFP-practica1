program sistema_inventario
    implicit none
    integer :: opcion
    character(len=256) :: inv, mov
    character(len=50) :: ubicacion
    integer :: total_equipos = 0

    ! Inventario
    character(len=50), dimension(100) :: nombres, ubicaciones
    integer, dimension(100) :: cantidades
    real, dimension(100) :: precios

    do
        print *, '------------------------------------------'
        print *, 'Sistema de Inventario'
        print *, '------------------------------------------'
        print *, '1. Cargar Inventario Inicial'
        print *, '2. Cargar Movimientos'
        print *, '3. Guardar Inventario'
        print *, '4. Salir'
        print *, 'Seleccione una opción:'
        read *, opcion

        select case (opcion)
        case (1)
            call cargar_inventario(inv, nombres, cantidades, precios, ubicaciones, total_equipos)
        case (2)
            call cargar_movimientos(mov, nombres, cantidades, ubicaciones, total_equipos)
        case (3)
            call guardar_inventario(inv, nombres, cantidades, precios, ubicaciones, total_equipos)
        case (4)
            print *, 'Saliendo...'
            exit
        case default
            print *, 'Opción no válida.'
        end select
    end do

contains 

subroutine cargar_inventario(archivo, nombres, cantidades, precios, ubicaciones, total)
    character(len=256), intent(inout) :: archivo
    character(len=50), dimension(100), intent(out) :: nombres, ubicaciones
    integer, dimension(100), intent(out) :: cantidades
    real, dimension(100), intent(out) :: precios
    integer, intent(inout) :: total
    integer :: estado, p1, p2, p3
    character(len=256) :: linea

    print *, 'Ingrese la ruta del archivo de inventario:'
    read *, archivo
    open(unit=10, file=trim(archivo), status='old', action='read', iostat=estado)

    if (estado /= 0) then
        print *, 'Error al abrir el archivo.'
        return
    endif

    total = 0
    do
        read(10, '(A)', iostat=estado) linea
        if (estado /= 0) exit

        p1 = index(linea, ";")
        p2 = index(linea(p1+1:), ";") + p1
        p3 = index(linea(p2+1:), ";") + p2

        nombres(total + 1) = adjustl(linea(14:p1-1))
        read(linea(p1+1:p2-1), *) cantidades(total + 1)
        read(linea(p2+1:p3-1), *) precios(total + 1)
        ubicaciones(total + 1) = adjustl(linea(p3+1:))
        total = total + 1
    end do

    close(10)
    print *, 'Inventario cargado exitosamente.'
end subroutine cargar_inventario

subroutine cargar_movimientos(archivo, nombres, cantidades, ubicaciones, total)
    character(len=256), intent(inout) :: archivo
    character(len=50), dimension(100), intent(inout) :: nombres, ubicaciones
    integer, dimension(100), intent(inout) :: cantidades
    integer, intent(inout) :: total
    integer :: estado, p1
    character(len=256) :: linea, comando
    character(len=50) :: nombre
    integer :: cantidad

    print *, 'Ingrese la ruta del archivo de movimientos:'
    read *, archivo
    open(unit=10, file=trim(archivo), status='old', action='read', iostat=estado)

    if (estado /= 0) then
        print *, 'Error al abrir el archivo.'
        return
    endif

    do
        read(10, '(A)', iostat=estado) linea
        if (estado /= 0) exit

        ! Ver el comando
        p1 = index(linea, " ")
        if (p1 == 0) then
            print *, 'Formato incorrecto en la línea:', trim(linea)
            cycle
        endif
        comando = adjustl(linea(1:p1-1))

        ! Sacar nombre, cantidad y ubicación
        linea = trim(linea(p1+1:))
        p1 = index(linea, ";")
        if (p1 == 0) then
            print *, 'Formato incorrecto en la línea:', trim(linea)
            cycle
        endif

        nombre = adjustl(linea(1:p1-1))
        linea = trim(linea(p1+1:))
        p1 = index(linea, ";")
        if (p1 == 0) then
            print *, 'Formato incorrecto en la línea:', trim(linea)
            cycle
        endif

        read(linea(1:p1-1), *, iostat=estado) cantidad
        if (estado /= 0) then
            print *, 'Error al leer la cantidad en la línea:', trim(linea)
            cycle
        endif

        ubicacion = adjustl(linea(p1+1:))

        if (trim(comando) == 'agregar_stock') then
            call agregar_stock(nombre, cantidad, nombres, cantidades, ubicaciones, total)
        else if (trim(comando) == 'eliminar_equipo') then
            call eliminar_equipo(nombre, cantidad, nombres, cantidades, ubicaciones, total)
        else
            print *, 'Comando no reconocido:', trim(comando)
        endif
    end do

    close(10)
    print *, 'Movimientos procesados exitosamente.'
end subroutine cargar_movimientos


subroutine agregar_stock(nombre, cantidad, nombres, cantidades, ubicaciones, total)
    character(len=50), intent(in) :: nombre
    integer, intent(in) :: cantidad
    character(len=50), dimension(100), intent(inout) :: nombres, ubicaciones
    integer, dimension(100), intent(inout) :: cantidades
    integer, intent(inout) :: total
    integer :: i
    logical :: encontrado = .false.

    do i = 1, total
        if (trim(nombres(i)) == trim(nombre)) then
            cantidades(i) = cantidades(i) + cantidad
            encontrado = .true.
            print *, 'Stock actualizado para', trim(nombre)
            exit
        endif
    end do

    if (.not. encontrado) then
        print *, 'Error: Equipo no encontrado:', trim(nombre)
    endif
end subroutine agregar_stock

subroutine eliminar_equipo(nombre, cantidad, nombres, cantidades, ubicaciones, total)
    character(len=50), intent(in) :: nombre
    integer, intent(in) :: cantidad
    character(len=50), dimension(100), intent(inout) :: nombres, ubicaciones
    integer, dimension(100), intent(inout) :: cantidades
    integer, intent(inout) :: total
    integer :: i
    logical :: encontrado = .false.

    do i = 1, total
        if (trim(nombres(i)) == trim(nombre)) then
            if (cantidades(i) >= cantidad) then
                cantidades(i) = cantidades(i) - cantidad
                print *, 'Cantidad reducida para', trim(nombre)
                if (cantidades(i) == 0) then
                    call eliminar_item(i, nombres, cantidades, ubicaciones, total)
                endif
                encontrado = .true.
                exit
            else
                print *, 'Error: Cantidad a eliminar mayor que la existente para:', trim(nombre)
                encontrado = .true.
                exit
            endif
        endif
    end do

    if (.not. encontrado) then
        print *, 'Error: Equipo no encontrado:', trim(nombre)
    endif
end subroutine eliminar_equipo

subroutine eliminar_item(index, nombres, cantidades, ubicaciones, total)
    integer, intent(in) :: index
    character(len=50), dimension(100), intent(inout) :: nombres, ubicaciones
    integer, dimension(100), intent(inout) :: cantidades
    integer, intent(inout) :: total
    integer :: i

    do i = index, total-1
        nombres(i) = nombres(i+1)
        cantidades(i) = cantidades(i+1)
        ubicaciones(i) = ubicaciones(i+1)
    end do

    total = total - 1
end subroutine eliminar_item

subroutine guardar_inventario(archivo, nombres, cantidades, precios, ubicaciones, total)
    character(len=256), intent(inout) :: archivo
    character(len=50), dimension(100), intent(in) :: nombres, ubicaciones
    integer, dimension(100), intent(in) :: cantidades
    real, dimension(100), intent(in) :: precios
    integer, intent(in) :: total
    integer :: i, estado
    integer :: contador_archivo

    save :: contador_archivo

    write(archivo, '(A, I3.3, A)') 'inventario_guardado', contador_archivo, '.txt'
    open(unit=20, file=trim(archivo), status='replace', action='write', iostat=estado)

    if (estado /= 0) then
        print *, 'Error al abrir el archivo para guardar el inventario.'
        return
    endif

    ! Encabezado del txt no borrar jsjs
    write(20, '(A)') 'Informe de Inventario:'
    write(20, '(A)') 'Equipo      Cantidad    Precio Unitario    Valor Total    Ubicación'
    write(20, '(A)') '--------------------------------------------------------------'

    do i = 1, total
        write(20, '(A10, I10, F10.2, F15.2, A12)') trim(nombres(i)), cantidades(i), precios(i), cantidades(i)*precios(i), trim(ubicaciones(i))
    end do

    close(20)
    print *, 'Inventario guardado exitosamente en ', trim(archivo)
    contador_archivo = contador_archivo + 1
end subroutine guardar_inventario

end program sistema_inventario