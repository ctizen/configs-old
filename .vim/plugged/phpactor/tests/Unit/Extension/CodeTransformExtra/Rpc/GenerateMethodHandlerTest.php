<?php

namespace Phpactor\Tests\Unit\Extension\CodeTransformExtra\Rpc;

use Phpactor\CodeTransform\Domain\Refactor\GenerateMethod;
use Phpactor\CodeTransform\Domain\SourceCode;
use Phpactor\Extension\CodeTransformExtra\Rpc\GenerateMethodHandler;
use Phpactor\Extension\Rpc\Handler;
use Phpactor\Extension\Rpc\Response\UpdateFileSourceResponse;
use Phpactor\Tests\Unit\Extension\Rpc\HandlerTestCase;

class GenerateMethodHandlerTest extends HandlerTestCase
{
    const EXAMPLE_SOURCE = '<php example source';
    const EXAMPLE_TRANSFORMED_SOURCE = '<php example source 1';
    const EXAMPLE_OFFSET = 1234;
    const EXAMPLE_PATH = '/path/to/1';


    /**
     * @var GenerateMethod
     */
    private $generateMethod;

    public function setUp(): void
    {
        $this->generateMethod = $this->prophesize(GenerateMethod::class);
    }

    public function testProvidesOriginalSourceFromDiskIfPathIsNotTheGivenPath(): void
    {
        $handler = $this->createHandler('generate_method');
        $source = SourceCode::fromStringAndPath(self::EXAMPLE_SOURCE, self::EXAMPLE_PATH);
        $transformedSource = SourceCode::fromStringAndPath(self::EXAMPLE_SOURCE, __FILE__);

        $this->generateMethod->generateMethod(
            $source,
            self::EXAMPLE_OFFSET
        )->willReturn($transformedSource);

        $response = $handler->handle([
            GenerateMethodHandler::PARAM_PATH => self::EXAMPLE_PATH,
            GenerateMethodHandler::PARAM_SOURCE => self::EXAMPLE_SOURCE,
            GenerateMethodHandler::PARAM_OFFSET => self::EXAMPLE_OFFSET,
        ]);

        $this->assertInstanceOf(UpdateFileSourceResponse::class, $response);
        assert($response instanceof UpdateFileSourceResponse);
        $this->assertEquals(__FILE__, $response->path());
        $this->assertEquals(file_get_contents(__FILE__), $response->oldSource());
        $this->assertEquals(self::EXAMPLE_SOURCE, $response->newSource());
    }

    public function testProvidesGivenSourceIfTransformedPathSameAsGivenPath(): void
    {
        $handler = $this->createHandler('generate_method');
        $source = SourceCode::fromStringAndPath(self::EXAMPLE_SOURCE, self::EXAMPLE_PATH);
        $transformedSource = SourceCode::fromStringAndPath(self::EXAMPLE_TRANSFORMED_SOURCE, self::EXAMPLE_PATH);

        $this->generateMethod->generateMethod(
            $source,
            self::EXAMPLE_OFFSET
        )->willReturn($transformedSource);

        $response = $handler->handle([
            GenerateMethodHandler::PARAM_PATH => self::EXAMPLE_PATH,
            GenerateMethodHandler::PARAM_SOURCE => self::EXAMPLE_SOURCE,
            GenerateMethodHandler::PARAM_OFFSET => self::EXAMPLE_OFFSET,
        ]);

        $this->assertInstanceOf(UpdateFileSourceResponse::class, $response);
        assert($response instanceof UpdateFileSourceResponse);
        $this->assertEquals(self::EXAMPLE_PATH, $response->path());
        $this->assertEquals(self::EXAMPLE_SOURCE, $response->oldSource());
        $this->assertEquals(self::EXAMPLE_TRANSFORMED_SOURCE, $response->newSource());
    }

    protected function createHandler(): Handler
    {
        return new GenerateMethodHandler($this->generateMethod->reveal());
    }
}
