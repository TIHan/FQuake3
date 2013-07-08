#include <m.h>

M_IMPORT MInstance *m_instance;

/*
==================
main
==================
*/
int
main (int argc, char *argv[])
{
	m_instance = m_instance_new ();

	// Initialize Mono
	m_init (m_instance, "Mono\\lib", "Mono\\etc", "FQuake3", M_RUNTIME_4_5);

	// Load System Assembly
	m_load_assembly (m_instance, "Engine.dll");

	// Invoke Start
	m_invoke_function_from_module (m_instance, "Engine", "System", "Init", NULL);

	// Cleanup
	m_instance_free (m_instance);

	return 0;
}