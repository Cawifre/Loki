namespace Loki

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System

type Entity =
    {
        Physics: Physics;
        Sprite: Sprite;
    }

type Sprite =
    {
        Texture: Texture2D;
        Size: Point;
        Offset: Point;
    }

    member this.Draw(position: Vector2, spriteBatch: SpriteBatch) =
        let sourceRectangle = Rectangle(this.Offset, this.Size)
        spriteBatch.Draw(this.Texture, position, Nullable.op_Implicit sourceRectangle, Color.White)

type Physics =
    {
        Position: Vector2;
        MovementDirection: Vector2;
        Speed: float32;
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

    let create(countX, countY, tileSizeX, tileSizeY, texture) =
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

    let draw (spriteBatch: SpriteBatch, tileSet: TileSet, tileLayer: TileLayer) =
        for y in 0 .. tileLayer.CountY do
            for x in 0 .. tileLayer.CountX do
                match getTileIndex x y tileLayer with
                | None -> ()
                | Some tileIndex -> 
                    if tileIndex = -1 then () else
                    let destination = Rectangle(x * tileSet.TileSizeX, y * tileSet.TileSizeY, tileSet.TileSizeX, tileSet.TileSizeY)
                    spriteBatch.Draw(tileSet.Texture, destination, Nullable.op_Implicit tileSet.Tiles.[tileIndex], Color.White)

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
        let inputDirection = getDirection(state)
        if inputDirection <> Vector2.Zero
        then
            inputDirection.Normalize()
            let dampening = 0.05f
            let adjustedMotion = initialVector + inputDirection * dampening
            adjustedMotion.Normalize()
            adjustedMotion
        else
            initialVector

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
               2;0;0;0;0;0;1;0;0;2;
               2;0;0;0;0;1;2;0;0;2;
               2;0;0;0;1;2;3;0;0;2;
               2;0;0;1;2;3;4;0;0;2;
               2;0;0;0;1;2;3;0;0;2;
               2;0;0;0;0;1;2;0;0;2;
               2;0;0;0;0;0;1;0;0;2;
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
                      Position = Vector2(64.f, 64.f)
                      Speed = 500.f
                      MovementDirection = Vector2(1.f, 0.f) }
                  Sprite = {
                      Texture = this.Content.Load "ball"
                      Size = Point(64, 64)
                      Offset = Point.Zero } }
 
    override this.Update (gameTime) =
        if (GamePad.GetState(PlayerIndex.One).Buttons.Back = ButtonState.Pressed || Keyboard.GetState().IsKeyDown(Keys.Escape))
        then this.Exit();

        let movementVector = getMovementVector(ball.Physics.MovementDirection, Keyboard.GetState())
        let newPostion =
            let maxX, maxY = float32 (tileLayer.CountX * tileSet.TileSizeX - ball.Sprite.Size.X), float32 (tileLayer.CountY * tileSet.TileSizeY - ball.Sprite.Size.Y)
            let position = ball.Physics.Position + movementVector * ball.Physics.Speed * float32 gameTime.ElapsedGameTime.TotalSeconds
            Vector2.Clamp(position, Vector2.Zero, Vector2(maxX, maxY))

        ball <- {ball with Physics = {ball.Physics with Position = newPostion; MovementDirection = movementVector}}

        base.Update(gameTime)
 
    override this.Draw (gameTime) =
        this.GraphicsDevice.Clear Color.DarkSlateGray

        spriteBatch.Begin()

        TileLayer.draw(spriteBatch, tileSet, tileLayer)
        ball.Sprite.Draw(ball.Physics.Position, spriteBatch)

        spriteBatch.End()

        base.Draw(gameTime)

