namespace Loki

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System
open MonoGame.Extended

module Math =

    let solveQuadratic a b c =
        let d = b * b - 4.f * a * c
        if a = 0.f || d < 0.f then
            None
        else
            let root1 = (-b + sqrt d) / 2.f / a
            let root2 = (-b - sqrt d) / 2.f / a
            Some(root1, root2)

type Sprite =
    {
        Texture: Texture2D;
        Size: Point;
        Offset: Point;
    }

    member this.Draw(center: Vector2, spriteBatch: SpriteBatch) =
        let topLeft = center - Vector2(float32 this.Size.X, float32 this.Size.Y) / 2.f
        let sourceRectangle = Rectangle(this.Offset, this.Size)
        spriteBatch.Draw(this.Texture, topLeft, Nullable.op_Implicit sourceRectangle, Color.White)

type BoundingBox =
    {
        Center: Vector2;
        CenterToCorner: Vector2;
        Rotation: float32;
    }

type BoundingCircle =
    {
        Center: Vector2;
        Radius: float32;
    }

type LineSegment =
    {
        A: Vector2;
        B: Vector2
    }

module LineSegment =

    let fromBounds (bounds: BoundingBox) =
        match bounds with
        | rotated when rotated.Rotation <> 0.f -> raise (NotImplementedException())
        | aabb ->
            [aabb.CenterToCorner
             aabb.CenterToCorner * Vector2(-1.f, 1.f)
             aabb.CenterToCorner * Vector2(-1.f, -1.f)
             aabb.CenterToCorner * Vector2(1.f, -1.f)
             aabb.CenterToCorner]
                |> List.map (fun (corner) -> aabb.Center + corner)
                |> List.pairwise
                |> List.map (fun (a, b) -> {A = a; B = b})

    let intersectSegment (a: LineSegment) (b: LineSegment) =
        let a1, a2 = a.A, a.B
        let b1, b2 = b.A, b.B
        let denominator = (b2.Y - b1.Y) * (a2.X - a1.X) - (b2.X - b1.X) * (a2.Y - a1.Y)
        if denominator = 0.f then None // segments are parallel
        else
            let uA = ((b2.X - b1.X) * (a1.Y - b1.Y) - (b2.Y - b1.Y) * (a1.X - b1.X)) / denominator
            let uB = ((a2.X - a1.X) * (a1.Y - b1.Y) - (a2.Y - a1.Y) * (a1.X - b1.X)) / denominator
            if uA < 0.f || uA > 1.f || uB < 0.f || uB > 1.f then None // intersection of lines is outside segment(s)
            else
                let x = a1.X + uA * (a2.X - a1.X)
                let y = a1.Y + uA * (a2.Y - a1.Y)
                let intersection = Vector2(x, y)
                Some(intersection, a, b)

    let intersectBox (box: BoundingBox) (segment: LineSegment) =
        let intersection =
            fromBounds box
            |> List.choose (intersectSegment segment)
            |> List.sortBy (fun (intersection, a, _) -> Vector2.Distance(a.A, intersection))
            |> List.tryHead
        match intersection with
        | None -> None
        | Some(intersectionPoint, _, { A = b1; B = b2 }) ->
            let tangent = Vector2.Normalize (b2 - b1)
            Some(tangent.PerpendicularCounterClockwise(), intersectionPoint)

    let intersectCircle (circle: BoundingCircle) (segment: LineSegment) =
        let translateToOrigin = Matrix2.CreateTranslation -circle.Center
        let reverseTranslate = Matrix2.CreateTranslation circle.Center

        let segmentAtOrigin = { segment with 
                                  A = translateToOrigin.Transform segment.A
                                  B = translateToOrigin.Transform segment.B
                              }

        // solve intersection between
        //  x^2 + y^2 = r^2
        // and
        //  y = mx + c
        let slope =
            (segmentAtOrigin.B - segmentAtOrigin.A)
            |> (fun v -> if v.X < 0.f then -v else v)
            |> (fun v -> v.Y / v.X)
        let yIntersect = segmentAtOrigin.A.Y - (slope * segmentAtOrigin.A.X)
        let roots = Math.solveQuadratic (slope * slope + 1.f) (2.f * yIntersect * slope) (yIntersect * yIntersect - circle.Radius * circle.Radius)

        let intersectAt (x: float32) =
            let y = slope * x + yIntersect
            let intersection = Vector2(x, y)
            let normal = Vector2.Normalize intersection
            (normal, reverseTranslate.Transform intersection)

        let onSegment (x: float32) =
            let min = min segmentAtOrigin.A.X segmentAtOrigin.B.X
            let max = max segmentAtOrigin.A.X segmentAtOrigin.B.X
            min < x && x < max

        match roots with
        | None -> None
        | Some(x1, x2) when x1 = x2 -> if onSegment x1 then Some(intersectAt x1) else None
        | Some(x1, x2) ->
            [x1; x2]
            |> List.filter onSegment
            |> List.map intersectAt
            |> List.sortBy (fun (_, p) -> Vector2.Distance(segment.A, p))
            |> List.tryHead

type BoundingShape =
    | BoundingBox of BoundingBox
    | BoundingCircle of BoundingCircle
    | LineSegment of LineSegment

    member this.Center =
        match this with
        | BoundingBox b -> b.Center
        | BoundingCircle c -> c.Center
        | LineSegment s -> Vector2((s.B.X - s.A.X) / 2.f, (s.B.Y - s.A.Y) / 2.f)

    member this.Sweep =
        match this with
        | BoundingBox b -> b.CenterToCorner.Length()
        | BoundingCircle c -> c.Radius
        | LineSegment s -> Vector2.Distance(this.Center, s.B)

    member this.Repositioned newCenter =
        match this with
        | BoundingBox b -> BoundingBox { Center = newCenter; CenterToCorner = b.CenterToCorner; Rotation = b.Rotation }
        | BoundingCircle c -> BoundingCircle { Center = newCenter; Radius = c.Radius }
        | LineSegment s ->
            let displacement = newCenter - this.Center
            LineSegment { A = s.A + displacement; B = s.B + displacement }

module BoundingShape =

    let explodeCircle radius (circle: BoundingCircle) =
        [BoundingCircle({ circle with Radius = circle.Radius + radius })]

    let explodeBox radius (box: BoundingBox) =
        if box.Rotation <> 0.f then raise (NotImplementedException())
        else
            let cornerCircles = [box.CenterToCorner
                                 box.CenterToCorner * Vector2(-1.f, 1.f)
                                 box.CenterToCorner * Vector2(-1.f, -1.f)
                                 box.CenterToCorner * Vector2(1.f, -1.f)]
                                |> List.map (fun corner -> BoundingCircle({ Center = box.Center + corner; Radius = radius }))

            let bottomRight, topLeft = box.Center + box.CenterToCorner,
                                       box.Center - box.CenterToCorner
            let bottomLeft, topRight = Vector2(box.Center.X - box.CenterToCorner.X, box.Center.Y + box.CenterToCorner.Y),
                                       Vector2(box.Center.X + box.CenterToCorner.X, box.Center.Y - box.CenterToCorner.Y)

            let segments = [Vector2(bottomRight.X, bottomRight.Y + radius), Vector2(bottomLeft.X, bottomLeft.Y + radius)
                            Vector2(bottomLeft.X - radius, bottomLeft.Y), Vector2(topLeft.X - radius, topLeft.Y)
                            Vector2(topLeft.X, topLeft.Y - radius), Vector2(topRight.X, topRight.Y - radius)
                            Vector2(topRight.X + radius, topRight.Y), Vector2(bottomRight.X + radius, bottomRight.Y)]
                           |> List.map (fun (a, b) -> LineSegment({ A = a; B = b }))

            List.append segments cornerCircles

    let explodeSegment radius (segment: LineSegment) =
        raise (NotImplementedException())
        [LineSegment(segment)] // TODO: Actually explode into two segments and two circle caps

    let explode radius (bounds: BoundingShape) =
        match bounds with
        | BoundingCircle c -> explodeCircle radius c
        | BoundingBox b -> explodeBox radius b
        | LineSegment s -> explodeSegment radius s

type Physics =
    {
        Bounds: BoundingShape;
        MovementDirection: Vector2;
        Speed: float32;
    }

type Contact = 
    {
        Intersection: Vector2;
        Normal: Vector2;
        ImmovableObject: BoundingShape;
        Physics: Physics;
        Movement: Vector2;
        TransformReversals: list<Matrix>;
    }

module Contact =

    let fromBox (physics: Physics) (movement: Vector2) (immovableObject: BoundingBox) =
        if immovableObject.Rotation <> 0.f then raise (NotImplementedException())
        else
            let movementSegment =
                {
                    A = physics.Bounds.Center;
                    B = physics.Bounds.Center + movement;
                }

            match LineSegment.intersectBox immovableObject movementSegment with
            | None -> None
            | Some(normal, intersection) -> Some({
                                                    Physics = physics;
                                                    Movement = movement;
                                                    ImmovableObject = BoundingBox(immovableObject);
                                                    Normal = normal;
                                                    Intersection = intersection;
                                                    TransformReversals = List.empty;
                                                })

    let fromCircle (physics: Physics) (movement: Vector2) (immovableObject: BoundingCircle) =
        let movementSegment =
            {
                A = physics.Bounds.Center;
                B = physics.Bounds.Center + movement;
            }

        match LineSegment.intersectCircle immovableObject movementSegment with
        | None -> None
        | Some(normal, intersection) -> Some({
                                                Physics = physics;
                                                Movement = movement;
                                                ImmovableObject = BoundingCircle(immovableObject);
                                                Normal = normal;
                                                Intersection = intersection;
                                                TransformReversals = List.empty;
                                             })

    let fromSegment (physics: Physics) (movement: Vector2) (immovableObject: LineSegment) =
        let movementSegment =
            {
                A = physics.Bounds.Center;
                B = physics.Bounds.Center + movement;
            }

        match LineSegment.intersectSegment movementSegment immovableObject with
        | None -> None
        | Some(intersection, _, b) -> Some({
                                                Physics = physics;
                                                Movement = movement;
                                                ImmovableObject = LineSegment(immovableObject);
                                                Normal = Vector2.Normalize ((b.B - b.A).PerpendicularCounterClockwise());
                                                Intersection = intersection;
                                                TransformReversals = List.empty;
                                           })

    let fromShape (physics: Physics) (movement: Vector2) (immovableObject: BoundingShape) =
        match immovableObject with
        | BoundingBox(box) -> fromBox physics movement box
        | BoundingCircle(circle) -> fromCircle physics movement circle
        | LineSegment(segment) -> fromSegment physics movement segment


type Entity =
    {
        Physics: Physics;
        Sprite: Sprite;
    }

type TileSet =
    {
        CountX: int
        CountY: int
        TileSizeX: int
        TileSizeY: int
        Texture: Texture2D
        Tiles: Rectangle array
    }

module TileSet =

    let create (countX, countY, tileSizeX, tileSizeY, texture) =
        let tiles = 
            [| for y in 0 .. countY - 1 do
                for x in 0 .. countX - 1 do
                    yield Rectangle(x * tileSizeX, y * tileSizeY, tileSizeX, tileSizeY) |]

        { CountX = countX
          CountY = countY
          TileSizeX = tileSizeX
          TileSizeY = tileSizeY
          Texture = texture
          Tiles = tiles }

    let tileToBounds x y (tileSet: TileSet) =
        let sweep = Vector2(float32 tileSet.TileSizeX, float32 tileSet.TileSizeY) / 2.f
        let center = Vector2(float32 (x * tileSet.TileSizeX), float32 (y * tileSet.TileSizeY)) + sweep
        BoundingBox {
                        Center = center;
                        CenterToCorner = sweep;
                        Rotation = 0.f;
                    }

type TileLayer =
    {
        CountX: int
        CountY: int
        Tiles: int array
    }

module TileLayer =

    let getTileIndex x y (layer: TileLayer) =
        // TODO: Check for out of range inputs and return `0` for the empty tile
        let layerIndex = y * layer.CountX + x
        match layer.Tiles |> Array.tryItem layerIndex with
        | Some tileId when tileId > 0 ->
            let tileIndex = tileId - 1 // Since `0` is the empty tile, all other ids are 1-based
            Some (tileIndex)
        | _ -> None

    let isFilled x y (layer: TileLayer) =
        let tileIndex = getTileIndex x y layer
        match tileIndex with
        | Some _ -> true
        | _ -> false

    let getTiles (min: Vector2) (max: Vector2) (tileSet: TileSet, layer: TileLayer) =

        let minX = (int min.X) / tileSet.TileSizeX
        let minY = (int min.Y) / tileSet.TileSizeY
        let maxX = (int (max.X + 0.5f)) / tileSet.TileSizeX
        let maxY = (int (max.Y + 0.5f)) / tileSet.TileSizeY

        [for x in minX .. maxX do
            for y in minY .. maxY do
                yield (isFilled x y layer, x, y)]

    let draw (spriteBatch: SpriteBatch, tileSet: TileSet, tileLayer: TileLayer) =
        for y in 0 .. tileLayer.CountY do
            for x in 0 .. tileLayer.CountX do
                match getTileIndex x y tileLayer with
                | None -> ()
                | Some tileIndex -> 
                    if tileIndex = -1 then () else
                    let destination = Rectangle(x * tileSet.TileSizeX, y * tileSet.TileSizeY, tileSet.TileSizeX, tileSet.TileSizeY)
                    spriteBatch.Draw(tileSet.Texture, destination, Nullable.op_Implicit tileSet.Tiles.[tileIndex], Color.White)

type CollisionResolution =
    {
        Contact: Contact;
        ResolvedPhysics: Physics;
        EventDistance: float32;
    }

module Collision =

    let reflect (contact: Contact) =
        let reflectedPhysics = { contact.Physics with
                                                 MovementDirection = Vector2.Reflect(contact.Physics.MovementDirection, contact.Normal);
                                                 Bounds = contact.Physics.Bounds.Repositioned(contact.Intersection)
                               }
        let distance = Vector2.Distance(contact.Physics.Bounds.Center, contact.Intersection)
        {
            Contact = contact;
            ResolvedPhysics = reflectedPhysics;
            EventDistance = distance;
        }

    let innerCollide (lastTickPhysics: Physics) (physics: Physics) (immovableObject: BoundingShape) =

        let movement = physics.Bounds.Center - lastTickPhysics.Bounds.Center
        match physics.Bounds with
        | BoundingBox(_) -> raise (NotImplementedException())
        | LineSegment(_) -> raise (NotImplementedException())
        | BoundingCircle(circle) ->
            BoundingShape.explode circle.Radius immovableObject
            |> List.choose (Contact.fromShape lastTickPhysics movement)
            |> List.choose (fun contact -> Some(reflect contact))

    let collide (tileLayer: TileLayer) (tileSet: TileSet) (lastTickPhysics: Physics) (physics: Physics) =

        let sweep = Vector2 physics.Bounds.Sweep
        let min = Vector2.Min(lastTickPhysics.Bounds.Center, physics.Bounds.Center) - sweep
        let max = Vector2.Max(lastTickPhysics.Bounds.Center, physics.Bounds.Center) + sweep
        
        let contacts =
          TileLayer.getTiles min max (tileSet, tileLayer)
          |> List.choose (fun (isFilled, x, y) -> if isFilled then Some(TileSet.tileToBounds x y tileSet) else None)
          |> List.collect (fun bounds -> innerCollide lastTickPhysics physics bounds)
          |> List.filter (fun resolution -> resolution.EventDistance > 0.f)
          |> List.sortBy (fun resolution -> resolution.EventDistance)

        List.tryHead contacts
        |> function
           | None -> physics
           | Some(resolution) -> resolution.ResolvedPhysics

type Game1 () as this =
    inherit Game()
 
    let graphics = new GraphicsDeviceManager(this)
    let mutable spriteBatch = Unchecked.defaultof<_>
    let mutable ball = Unchecked.defaultof<Entity>
    let mutable tileSet = Unchecked.defaultof<TileSet>
    let mutable tileLayer = Unchecked.defaultof<TileLayer>
    let mutable fonts = Unchecked.defaultof<Map<string, SpriteFont>>

    let defaultBallPhysics = { Bounds = BoundingCircle({ Center = Vector2(32.f + 1.f * 64.f,32.f + 1.f * 64.f); Radius = 32.f })
                               Speed = 300.f
                               MovementDirection = Vector2.Normalize(Vector2(1.f, 7.f)) }

    let (|KeyDown|_|) k (state: KeyboardState) =
        if state.IsKeyDown k then Some() else None

    let getDirection (state) =
        let xDirection =
            match state with
                | KeyDown Keys.A | KeyDown Keys.Left -> -1.f
                | KeyDown Keys.D | KeyDown Keys.Right -> 1.f
                | _ -> 0.f
        let yDirection =
            match state with
                | KeyDown Keys.W | KeyDown Keys.Up -> -1.f
                | KeyDown Keys.S | KeyDown Keys.Down -> 1.f
                | _ -> 0.f
        Vector2(xDirection, yDirection)
    
    let getMovementVector (initialVector, state: KeyboardState) =
        let inputDirection = getDirection state
        if inputDirection <> Vector2.Zero
        then
            inputDirection.Normalize()
            let dampening = 0.05f
            let adjustedMotion = initialVector + inputDirection * dampening
            Vector2.Normalize adjustedMotion
        else
            Vector2.Normalize initialVector

    do
        this.Content.RootDirectory <- "Content"
        this.IsMouseVisible <- true

    override this.Initialize() = 
        graphics.PreferredBackBufferWidth <- 10 * 64
        graphics.PreferredBackBufferHeight <- 15 * 64
        graphics.ApplyChanges()

        let blackAndWhiteBlocks = this.Content.Load "BlackAndWhiteBlocks"
        tileSet <- TileSet.create(2, 2, 64, 64, blackAndWhiteBlocks)

        let layerTiles = 
            [| 2;2;2;2;2;2;2;2;2;2;
               2;0;0;0;0;0;0;0;0;2;
               2;0;0;0;0;0;0;0;0;2;
               2;0;0;0;0;0;4;0;0;2;
               2;0;0;0;0;0;0;0;0;2;
               2;0;0;1;0;0;0;0;0;2;
               2;0;0;0;0;0;0;0;0;2;
               2;0;0;0;0;0;0;0;0;2;
               2;0;0;0;0;0;0;0;0;2;
               2;0;0;0;0;0;0;0;0;2;
               2;0;0;0;0;0;0;1;0;2;
               2;0;0;0;0;0;0;0;0;2;
               2;0;0;3;0;0;0;0;0;2;
               2;0;0;0;0;0;0;0;0;2;
               2;2;2;2;2;2;2;2;2;2 |]
        tileLayer <- {
            CountX = 10
            CountY = 15
            Tiles = layerTiles
        }

        base.Initialize()

    override this.LoadContent() =
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)

        fonts <- Map([
                    "consolas12", this.Content.Load "Consolas12"
                    ])

        ball <- { Physics = defaultBallPhysics
                  Sprite = {
                      Texture = this.Content.Load "ball"
                      Size = Point(64, 64)
                      Offset = Point.Zero } }
 
    override this.Update (gameTime) =
        let keyboardState = Keyboard.GetState()
        if (GamePad.GetState(PlayerIndex.One).Buttons.Back = ButtonState.Pressed || keyboardState.IsKeyDown(Keys.Escape))
        then this.Exit();

        let resetBall = keyboardState.IsKeyDown(Keys.R)
        let physics = if resetBall then defaultBallPhysics else ball.Physics

        let movementDirection = getMovementVector(physics.MovementDirection, keyboardState)

        let newPosition =
            let maxX, maxY = float32 (tileLayer.CountX * tileSet.TileSizeX), float32 (tileLayer.CountY * tileSet.TileSizeY)
            let position = physics.Bounds.Center + movementDirection * physics.Speed * float32 gameTime.ElapsedGameTime.TotalSeconds
            Vector2.Clamp(position, Vector2.Zero, Vector2(maxX, maxY))

        let proposedPhysics = { physics with
                                        Bounds = physics.Bounds.Repositioned newPosition
                                        MovementDirection = movementDirection }
        let newPhysics = Collision.collide tileLayer tileSet physics proposedPhysics

        ball <- { ball with Physics = newPhysics }

        base.Update(gameTime)
 
    override this.Draw (gameTime) =
        this.GraphicsDevice.Clear Color.DarkSlateGray

        spriteBatch.Begin()

        TileLayer.draw(spriteBatch, tileSet, tileLayer)
        ball.Sprite.Draw(ball.Physics.Bounds.Center, spriteBatch)

        let debugInfo = String.Join("\n",
                            [
                                sprintf "Ball.X: %f" ball.Physics.Bounds.Center.X
                                sprintf "Ball.Y: %f" ball.Physics.Bounds.Center.Y
                            ])
        spriteBatch.DrawString(fonts.["consolas12"], debugInfo, Vector2.One, Color.White)

        spriteBatch.End()

        base.Draw(gameTime)

