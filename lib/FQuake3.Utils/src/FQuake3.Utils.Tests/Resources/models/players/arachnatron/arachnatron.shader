models/players/arachnatron/body
{
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

models/players/arachnatron/legs
{
	{
		map models/players/arachnatron/legs.tga
		blendfunc blend
		rgbGen lightingDiffuse
	}
	{
		map models/players/arachnatron/legsfx.tga
		blendfunc add
		rgbGen wave sin 0 1 0 1 
	}
}

models/players/arachnatron/blue_h
{
	{
		map models/players/arachnatron/blue_h.tga
		rgbGen lightingDiffuse
	}
	{
		map textures/effects/quadmap2.tga
		blendfunc add
		rgbGen identity
		tcMod turb 0 1 0 1
		tcMod scale 2 2
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
		rgbGen wave sin 0 1 0 1 
	}
}

models/players/arachnatron/blue_legs
{
	{
		map models/players/arachnatron/blue_legs.tga
		blendfunc blend
		rgbGen lightingDiffuse
	}
	{
		map models/players/arachnatron/blue_legsfx.tga
		blendfunc add
		rgbGen wave sin 0 1 0 1 
	}
}

models/players/arachnatron/red_h
{
	{
		map models/players/arachnatron/red_h.tga
		rgbGen lightingDiffuse
	}
	{
		map textures/sfx/dust_puppy2.tga
		blendfunc add
		rgbGen const ( 1 0.309804 0.309804 )
		tcMod turb 0 1 0 1
		tcMod scale 2 2
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
		rgbGen wave sin 0 1 0 1 
	}
}

models/players/arachnatron/red_legs
{
	{
		map models/players/arachnatron/red_legs.tga
		blendfunc blend
		rgbGen lightingDiffuse
	}
	{
		map models/players/arachnatron/red_legsfx.tga
		blendfunc add
		rgbGen wave sin 0 1 0 1 
	}
}

