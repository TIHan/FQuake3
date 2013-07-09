#include <m.h>

/*
==================
main
==================
*/
int
main (int argc, char *argv[])
{
	// Initialize Mono Domain
	MDomain *const domain = m_domain_new ("Mono\\lib", "Mono\\etc", "FQuake3", M_RUNTIME_4_5);

	// Load System Assembly
	m_load_assembly ("Engine.dll");

	// Invoke Start
	m_invoke_function_from_module ("Engine", "Engine", "System", "Init", NULL);

	// Cleanup
	m_domain_free (domain);

	return 0;
}