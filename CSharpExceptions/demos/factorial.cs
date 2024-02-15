class Program
{
    int Fac(int num)
    {
        if (num == 1)
        {
            return 1;
        }
        else 
        {
            return num * Fac(num - 1);
        }
    }

    static void Main() {}
}