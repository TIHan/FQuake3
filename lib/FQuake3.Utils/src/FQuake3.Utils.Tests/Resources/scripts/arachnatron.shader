models/players/arachnatron/head
{
deformVertexes wave 17 sin .1 .1 0 .9
	{
		map models/players/arachnatron/head.tga
		rgbGen lightingDiffuse
	}
	{
		map textures/effects/tinfx2c.tga
		blendfunc add
		rgbGen identity
		tcMod scale 3 3
		tcGen environment 
	}
	{
		map models/players/arachnatron/head.tga
		blendfunc blend
		rgbGen lightingDiffuse
	}
	{
		map models/players/arachnatron/headfx.tga
		blendfunc add
		rgbGen wave sin 0 1 0 1 
	}
}


models/players/arachnatron/body
{
deformVertexes wave 6 sin .1 .15 0 .9
	{
		map models/players/arachnatron/head.tga
		rgbGen lightingDiffuse
	}
	{
		map textures/effects/tinfx2c.tga
		blendfunc add
		rgbGen identity
		tcMod scale 3 3
		tcGen environment 
	}
	{
		map models/players/arachnatron/head.tga
		blendfunc blend
		rgbGen lightingDiffuse
	}
	{
		map models/players/arachnatron/headfx.tga
		blendfunc add
		rgbGen wave sin 0 1 0 .9
	}
}

models/players/arachnatron/legs
{
	{
		map models/players/arachnatron/legs.tga
		alphafunc GE128
		rgbGen lightingDiffuse
	}
	{
		map models/players/arachnatron/legsfx.tga
		blendfunc add
		rgbGen wave sin 0 1 0 1 
	}
}

models/players/arachnatron/b_head
{
deformVertexes wave 21 sin .1 .1 .6 1.1

	{
		map models/players/arachnatron/blue_h.tga
		rgbGen lightingDiffuse
	}
	{
		map textures/effects/tinfx2c.tga
		blendfunc add
		rgbGen identity
		tcMod scale 3 3
		tcGen environment 
	}
	{
		map models/players/arachnatron/blue_h.tga
		blendfunc blend
		rgbGen lightingDiffuse
	}
	{
		map models/players/arachnatron/headfx.tga
		blendfunc add
		rgbGen wave sin 0 1 0 1.1 
	}
}

models/players/arachnatron/blue_h
{
deformVertexes wave 6 sin .1 .15 0 .9
	{
		map models/players/arachnatron/blue_h.tga
		rgbGen lightingDiffuse
	}
	{
		map textures/effects/tinfx2c.tga
		blendfunc add
		rgbGen identity
		tcMod scale 3 3
		tcGen environment 
	}
	{
		map models/players/arachnatron/blue_h.tga
		blendfunc blend
		rgbGen lightingDiffuse
	}
	{
		map models/players/arachnatron/headfx.tga
		blendfunc add
		rgbGen wave sin 0 1 0 1.1 
	}
}

models/players/arachnatron/blue_legs
{
	{
		map models/players/arachnatron/blue_legs.tga
		alphafunc GE128
		rgbGen lightingDiffuse
	}
	{
		map models/players/arachnatron/blue_legsfx.tga
		blendfunc add
		rgbGen wave sin 0 1 0 1 
	}
}

models/players/arachnatron/r_head
{
deformVertexes wave 11 sin .1 .1 .3 1.2
	{
		map models/players/arachnatron/red_h.tga
		rgbGen lightingDiffuse
	}
	{
		map textures/effects/tinfx2c.tga
		blendfunc add
		rgbGen identity
		tcMod scale 3 3
		tcGen environment 
	}
	{
		map models/players/arachnatron/red_h.tga
		blendfunc blend
		rgbGen lightingDiffuse
	}
	{
		map models/players/arachnatron/red_hfx.tga
		blendfunc add
		rgbGen wave sin 0 1 0 1.3
	}
}

models/players/arachnatron/red_h
{
deformVertexes wave 6 sin .1 .15 0 .9
	{
		map models/players/arachnatron/red_h.tga
		rgbGen lightingDiffuse
	}
	{
		map textures/effects/tinfx2c.tga
		blendfunc add
		rgbGen identity
		tcMod scale 3 3
		tcGen environment 
	}
	{
		map models/players/arachnatron/red_h.tga
		blendfunc blend
		rgbGen lightingDiffuse
	}
	{
		map models/players/arachnatron/red_hfx.tga
		blendfunc add
		rgbGen wave sin 0 1 0 1.2
	}
}

models/players/arachnatron/red_legs
{
	{
		map models/players/arachnatron/red_legs.tga
		alphafunc GE128
		rgbGen lightingDiffuse
	}
	{
		map models/players/arachnatron/red_legsfx.tga
		blendfunc add
		rgbGen wave sin 0 1 0 1 
	}
}

