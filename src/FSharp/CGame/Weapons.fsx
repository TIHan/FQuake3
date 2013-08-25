#r @"..\..\..\build\Engine.dll"
#r @"..\..\..\build\Engine.Renderer.dll"
#r @"..\..\..\build\CGame.dll"

namespace CGame

open Engine.Math
open Engine.Renderer

module Weapons =
    /// <summary>
    /// Based on Q3: CG_CalculateWeaponPosition
    /// CalculateWeaponPosition
    /// </summary>
    let calculateWeaponPosition (cg: CGame) =
        let origin = cg.RefDef.ViewOrigin
        let angles = cg.RefDefViewAngles

        // on odd legs, invert some angles
        let scale =
            match cg.BobCycle &&& 1 with
            | 0 -> -cg.XYSpeed
            | _ -> cg.XYSpeed

        // gun angles from bobbing
        let angles =
            Vector3 (
                angles.X + (cg.XYSpeed * cg.BobFractionSin * 0.005f), // PITCH
                angles.Y + (scale * cg.BobFractionSin * 0.01f), // YAW
                angles.Z + (scale * cg.BobFractionSin * 0.005f) // ROLL
            )


        let deltaTime = cg.Time - cg.LandTime
        
        let originZ =
            // drop the weapon when landing
            match deltaTime with
            | x when x < Constants.LandDeflectTime ->
                origin.Z + (cg.LandChange * 0.25f * (single x / single Constants.LandDeflectTime))
            | x when x < Constants.LandDeflectTime + Constants.LandReturnTime ->
                origin.Z + (cg.LandChange * 0.25f * single (Constants.LandDeflectTime + Constants.LandReturnTime - x) / single Constants.LandReturnTime)
            | _ -> origin.Z

        // idle drift
        let scale = cg.XYSpeed + 40.f
        let fractionSin = sin <| single cg.Time * 0.001f
        let angles =
            Vector3 (
                angles.X + (scale * fractionSin * 0.01f), // PITCH
                angles.Y + (scale * fractionSin * 0.01f), // YAW
                angles.Z + (scale * fractionSin * 0.01f) // ROLL
            )
        let origin =
            Vector3 (
                origin.X,
                origin.Y,
                originZ
            )
        (origin, angles)

