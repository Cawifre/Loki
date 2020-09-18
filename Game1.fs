namespace Loki

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System

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

type BoundingShape =
    | BoundingBox of BoundingBox
    | BoundingCircle of BoundingCircle

    member this.Center =
        match this with
        | BoundingBox(b) -> b.Center
        | BoundingCircle(c) -> c.Center

    member this.Sweep =
        match this with
        | BoundingBox(b) -> b.CenterToCorner.Length()
        | BoundingCircle(c) -> c.Radius

    member this.Repositioned newCenter =
        match this with
        | BoundingBox(b) -> BoundingBox { Center = newCenter; CenterToCorner = b.CenterToCorner; Rotation = b.Rotation }
        | BoundingCircle(c) -> BoundingCircle { Center = newCenter; Radius = c.Radius }

type Physics =
    {
        Bounds: BoundingShape;
        MovementDirection: Vector2;
        Speed: float32;
    }

type Contact = 
    {
        Physics: Physics;
        Movement: Vector2;
        ImmovableObject: BoundingBox;
        Normal: Vector2;
        TransformReversals: seq<Matrix>;
    }

module Contact =

    let normal (movement: Vector2) (box: BoundingBox) =
        Vector2.Negate movement |> Vector2.Normalize // TODO: Stop lying

    let create (physics: Physics) (movement: Vector2) (immovableObject: BoundingShape) =
        match immovableObject with
        | BoundingCircle(_) -> raise (NotImplementedException())
        | BoundingBox(b) when b.Rotation <> 0.f -> raise (NotImplementedException())
        | BoundingBox(b) -> let normal = normal movement b
                            {
                                Physics = physics;
                                Movement = movement;
                                ImmovableObject = b;
                                Normal = normal;
                                TransformReversals = Seq.empty
                            }

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

module Collision =

    let reflect (contact: Contact) =
        let reflectedPhysics = { contact.Physics with 
                                                 MovementDirection = Vector2.Reflect(contact.Physics.MovementDirection, contact.Normal);
                               }
        let distance = Vector2.Distance(contact.Physics.Bounds.Center, contact.ImmovableObject.Center)
        (contact, reflectedPhysics, distance)

    let innerCollide (lastTickPhysics: Physics) (physics: Physics) (immovableObject: BoundingShape) =

        let movement = physics.Bounds.Center - lastTickPhysics.Bounds.Center
        let contact = Contact.create lastTickPhysics movement immovableObject
        Some(reflect contact)

    let collide (tileLayer: TileLayer) (tileSet: TileSet) (lastTickPhysics: Physics) (physics: Physics) =

        let sweep = Vector2 physics.Bounds.Sweep
        let min = Vector2.Min(lastTickPhysics.Bounds.Center, physics.Bounds.Center) - sweep
        let max = Vector2.Max(lastTickPhysics.Bounds.Center, physics.Bounds.Center) + sweep
        
        TileLayer.getTiles min max (tileSet, tileLayer)
        |> Seq.choose (fun (isFilled, x, y) -> if isFilled then Some(TileSet.tileToBounds x y tileSet) else None)
        |> Seq.choose(fun (bounds) -> innerCollide lastTickPhysics physics bounds)
        |> Seq.sortBy(fun (_, _, distance) -> distance)
        |> Seq.tryHead
        |> function
           | None -> physics
           | Some(_, newPhysics, _) -> newPhysics

type Game1 () as this =
    inherit Game()
 
    let graphics = new GraphicsDeviceManager(this)
    let mutable spriteBatch = Unchecked.defaultof<_>
    let mutable ball = Unchecked.defaultof<Entity>
    let mutable tileSet = Unchecked.defaultof<TileSet>
    let mutable tileLayer = Unchecked.defaultof<TileLayer>

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
               2;0;0;0;0;0;0;0;0;2;
               2;0;0;0;0;0;0;0;0;2;
               2;0;0;0;0;0;0;0;0;2;
               2;0;0;0;0;0;0;0;0;2;
               2;0;0;0;0;0;0;0;0;2;
               2;0;0;0;0;0;0;0;0;2;
               2;0;0;0;0;0;0;0;0;2;
               2;0;0;0;0;0;0;0;0;2;
               2;0;0;0;0;0;0;0;0;2;
               2;0;0;0;0;0;0;0;0;2;
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

        ball <- { Physics = {
                      Bounds = BoundingCircle({ Center = Vector2(96.f, 96.f); Radius = 32.f })
                      Speed = 500.f
                      MovementDirection = Vector2.Normalize Vector2.One }
                  Sprite = {
                      Texture = this.Content.Load "ball"
                      Size = Point(64, 64)
                      Offset = Point.Zero } }
 
    override this.Update (gameTime) =
        let keyboardState = Keyboard.GetState()
        if (GamePad.GetState(PlayerIndex.One).Buttons.Back = ButtonState.Pressed || keyboardState.IsKeyDown(Keys.Escape))
        then this.Exit();

        let physics = ball.Physics

        let movementDirection = getMovementVector(physics.MovementDirection, keyboardState)

        let newPosition =
            let maxX, maxY = float32 (tileLayer.CountX * tileSet.TileSizeX), float32 (tileLayer.CountY * tileSet.TileSizeY)
            let position = physics.Bounds.Center + movementDirection * physics.Speed * float32 gameTime.ElapsedGameTime.TotalSeconds
            Vector2.Clamp(position, Vector2.Zero, Vector2(maxX, maxY))

        let proposedPhysics = { physics with
                                        Bounds = physics.Bounds.Repositioned newPosition
                                        MovementDirection = movementDirection }
        let newPhysics = Collision.collide tileLayer tileSet ball.Physics proposedPhysics

        ball <- { ball with Physics = newPhysics }

        base.Update(gameTime)
 
    override this.Draw (gameTime) =
        this.GraphicsDevice.Clear Color.DarkSlateGray

        spriteBatch.Begin()

        TileLayer.draw(spriteBatch, tileSet, tileLayer)
        ball.Sprite.Draw(ball.Physics.Bounds.Center, spriteBatch)

        spriteBatch.End()

        base.Draw(gameTime)

