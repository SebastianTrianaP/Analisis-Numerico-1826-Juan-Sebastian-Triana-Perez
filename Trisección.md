**Metodo de Trisección**

![Grafica problema](https://a34f2e3d-a-62cb3a1a-s-sites.googlegroups.com/site/g03metodosnumericos2012/unidad-ii/Nueva%20imagen%20%283%29.bmp?attachauth=ANoY7crcglfRfDUnk1Wx53YDIjSPzdU9Y6yDfqUXVgTYw4nqoZ9bsmZI58Qw8HVVtEPlFpZkMZz6rtTPSJftf3ii64aDnBIgJAdL1LlugoU_zGRUA97S9O55Na3Zy_JQZOOcQNoc1Xu0q3QbFKIbu4knTumZcdTC4jXJ9pPv2YbiBhbK73XHFOxJK3y8aDhTol5sydq34873Jd8Um2O1oswIgL56e-Ga0kufnc4dIQiyFQECsueqvDtTDcwGWMiUzFkYGIKjk79g&attredirects=0)

Dada la grafica podemos saber mas o menos por donde es que se encuentra la raiz, en el caso particular del problema es en el intervalo [10,20], el programa tabulara los valores para que el usuario seleccione un intervalo mas ajustado, despues mostrara los pasos de como se realizo la trisección y como se fue reduciendo el intervalo en el que se encuentra la raiz.
A continuacion se presenta el codigo del programa:

    #include <iostream>
    #include <iomanip>
    #include <cmath>
    #define PRECISION 6
    
    using namespace std;
    void tabulacion (double a, double b);
    double f(double a);
    int main()
    {
        double a ,b, tolerancia = 0.00000001;
        cout <<setprecision(PRECISION);
        cout << "\t\tFuncion para calcular la raices aplicando el metodo de triseccion" << endl;
        cout<< "Funcion: (667.38/x)*(1-e^(-0.146843*x))-40"<<endl;
        cout << "El intervalo incial esta dado por: "<<endl;
        cout << "a = ";
        cin>>a;
        cout << "b = ";
        cin>>b;
        cout << "Se trabajara con una precision de: "<<PRECISION<<endl;
        tabulacion(a,b);
        cout << "Elija el valor inferior y superior que encierra la raiz de forma tal que la funcion cambie de signo en el intervalo" <<endl;
        cout << "a= ";
        cin>>a;
        cout <<"b= ";
        cin>>b;
        double xr1,xr2;
        if (f(a)*f(b)>0)
        {
            cout << "No se puede aplicar el metodo de la triseccion"<< endl;
            cout << "Porque f("<< a << ") y f("<< b << ") tienes el mismo signo" <<endl;
        }
        else
        {
            cout<<"Tolerancia = "<<tolerancia;
            cout <<"\na\tb\tx1\tx2\t f(a)\t\tf(b)\t\tf(x1)\t\tf(x2)\n"<<endl;
            do
            {
                xr1=((2.0*a)+b)/3.0;
                xr2=(a+(2.0*b))/3.0;
                cout<<a<<"\t"<<b<<"\t"<<xr1<<"\t"<<xr2<<"   "<<f(a)<<"\t"<<f(b)<<"\t"<<f(xr1)<<"\t"<<f(xr2)<<endl;
    
                if((abs(f(xr1)) <= tolerancia) || (abs(f(xr2)) <= tolerancia))
                {
                    if (abs(f(xr1)) <= tolerancia){
                    cout<<"Para una tolerancia de "<<tolerancia<<" la raiz de f es: "<<xr1<<endl;
                    break;
                    }
                    if (abs(f(xr2)) <= tolerancia){
                    cout<<"Para una tolerancia de "<<tolerancia<<" la raiz de f es: "<<xr2<<endl;
                    break;
                    }
                }
                else
                {
                    if (f(xr1)*f(a)<0)
                    {
                        b=xr1;
                    }
                    else if (f(xr1)*f(xr2)<0)
                    {
                        a=xr1;
                        b=xr2;
                    }
                    else if (f(xr2)*f(b)<0)
                    {
                        a=xr2;
                    }
                }
            }
            while(1);
    
    
        }
        return 0;
    }
    void tabulacion (double a, double b)
    {
        int puntos = 11;
        double ancho = (b-a)/10.0;
        cout<<"\tx\tf(x) "<<endl;
        for (int i =0 ; i< puntos; i++)
        {
            cout<<"\t"<<a<<"\t"<<f(a)<<endl;
            a = a +ancho;
        }
    }
    double f(double a)
    {
        return (667.38/a)*(1-exp(-0.146843*a))-40;
    }



