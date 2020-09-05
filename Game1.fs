namespace Loki

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System

type Sprite = 
    {
        Position: Vector2;
        MovementVector: Vector2;
        Speed: float32;
        Texture: Texture2D;
        Size: Point;
        Offset: Point;
    }

    member this.Draw(spriteBatch: SpriteBatch) =
        let sourceRectangle = Rectangle(this.Offset, this.Size)
        spriteBatch.Draw(this.Texture, this.Position, Nullable.op_Implicit sourceRectangle, Color.White)

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
    let mutable ballTexture = Unchecked.defaultof<Texture2D>
    let mutable ball = Unchecked.defaultof<Sprite>
    let mutable tileSet = Unchecked.defaultof<TileSet>
    let mutable tileLayer = Unchecked.defaultof<TileLayer>

    let (|KeyDown|_|) k (state: KeyboardState) =
        if state.IsKeyDown k then Some() else None

    let getMotion = function
        | (KeyDown Keys.A | KeyDown Keys.Left) & (KeyDown Keys.W | KeyDown Keys.Up) -> Vector2(-1.f, -1.f)
        | (KeyDown Keys.A | KeyDown Keys.Left) & (KeyDown Keys.S | KeyDown Keys.Down) -> Vector2(-1.f, 1.f)
        | (KeyDown Keys.D | KeyDown Keys.Right) & (KeyDown Keys.W | KeyDown Keys.Up) -> Vector2(1.f, -1.f)
        | (KeyDown Keys.D | KeyDown Keys.Right) & (KeyDown Keys.S | KeyDown Keys.Down) -> Vector2(1.f, 1.f)
        | KeyDown Keys.A | KeyDown Keys.Left -> Vector2(-1.f, 0.f)
        | KeyDown Keys.D | KeyDown Keys.Right -> Vector2(1.f, 0.f)
        | KeyDown Keys.S | KeyDown Keys.Down -> Vector2(0.f, 1.f)
        | KeyDown Keys.W | KeyDown Keys.Up -> Vector2(0.f, -1.f)
        | _ -> Vector2.Zero
    
    let getMovementVector (initialVector, state: KeyboardState) =
        let motion = getMotion(state)
        if motion <> Vector2.Zero
        then
            motion.Normalize()
            motion
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

        ballTexture <- this.Content.Load "ball"

        ball <- { Position = Vector2(64.f, 64.f)
                  Speed = 500.f
                  MovementVector = Vector2(1.f, 0.f)
                  Texture = ballTexture
                  Size = Point(64, 64)
                  Offset = Point.Zero }
 
    override this.Update (gameTime) =
        if (GamePad.GetState(PlayerIndex.One).Buttons.Back = ButtonState.Pressed || Keyboard.GetState().IsKeyDown(Keys.Escape))
        then this.Exit();

        let movementVector = getMovementVector(ball.MovementVector, Keyboard.GetState())

        let newPostion =
            let maxX, maxY = float32 (tileLayer.CountX * tileSet.TileSizeX - ball.Size.X), float32 (tileLayer.CountY * tileSet.TileSizeY - ball.Size.Y)
            let position = ball.Position + movementVector * ball.Speed * float32 gameTime.ElapsedGameTime.TotalSeconds
            Vector2.Clamp(position, Vector2.Zero, Vector2(maxX, maxY))

        ball <- {ball with Position = newPostion; MovementVector = movementVector}

        base.Update(gameTime)
 
    override this.Draw (gameTime) =
        this.GraphicsDevice.Clear Color.DarkSlateGray

        spriteBatch.Begin()

        TileLayer.draw(spriteBatch, tileSet, tileLayer)
        ball.Draw(spriteBatch)

        spriteBatch.End()

        base.Draw(gameTime)

