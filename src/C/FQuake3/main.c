#include <m.h>

__declspec(dllimport) MInstance *m_instance;

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
	m_init (m_instance, "Mono-3.0.9\\lib", "Mono-3.0.9\\etc", "FQuake3", M_RUNTIME_4_0);

	// Load System Assembly
	m_load_assembly (m_instance, "idTech3.System.dll");

	// Invoke Start
	m_invoke_function_from_module (m_instance, "idTech3", "idSystem", "Init", NULL);

	// Cleanup
	m_cleanup (m_instance);

	return 0;
}