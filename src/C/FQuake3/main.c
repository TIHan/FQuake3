#include <m.h>

static GOptionEntry option_entries[] =
{
	{ "mono-lib", 'l', 0, G_OPTION_ARG_STRING, "Mono\\lib", "Mono lib directory path", "ML" },
	{ "mono-etc", 'e', 0, G_OPTION_ARG_STRING, "Mono\\etc", "Mono etc directory path", "ME" },
	{ NULL }
};

/*
==================
main
==================
*/
int
main (int argc, char *argv[])
{
	GOptionContext* option_context = g_option_context_new ("- options for mono");
	GError *error = NULL;
	MDomain* domain;
	
	g_option_context_add_main_entries (option_context, option_entries, NULL);

	if (!g_option_context_parse (option_context, &argc, &argv, &error))
	{
		g_print ("Invalid option: %s\n", error->message);
		return 1;
	}

	// Initialize Mono Domain
	domain = m_domain_new ((const gchar*)option_entries [0].arg_data, (const gchar*)option_entries [0].arg_data, "FQuake3", M_RUNTIME_4_5);

	// Load FSharp.Core Assembly
	m_load_assembly ("FSharp.Core.dll");

	// Load Engine Assembly
	m_load_assembly ("Engine.dll");

	// Invoke Engine Init
	m_invoke_method ("Engine", "Engine", "System", "Init", NULL);

	// Free Domain
	m_domain_free (domain);

	// Free Option Context
	g_option_context_free (option_context);

	return 0;
}