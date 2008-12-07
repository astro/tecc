#define LIMIT 2000000

int main()
{
  char primes[LIMIT];
  int i, n = 0;
  register long long sum = 0;

  for(i = 2; i < LIMIT; ++i)
  {
    primes[i] = 1;
  }
  for(i = 2; i < LIMIT; ++i)
  {
    if (primes[i])
    {
      int j;
      if (n % 100 == 0)
        printf("%i\n", i);
      n++;
      sum += i;
      for(j = i * 2; j < LIMIT; j += i)
      {
        if (j % i == 0)
          primes[j] = 0;
      }
    }
  }
  printf("Sum: %lli\n", sum);
}
