#ifdef FOO
#  if FOO(1, 2)
#    error This should not happen
#  elif FOO(2, 3)
#    error Not this either
#  endif
#endif

int main(void)
{
	return 0;
}
