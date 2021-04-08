<?php

namespace Phpactor\Tests\Unit\Extension\CodeTransformExtra\Rpc;

use Phpactor\Extension\Rpc\Handler;
use Phpactor\Extension\Rpc\Response\InputCallbackResponse;
use Phpactor\CodeTransform\Domain\SourceCode;
use Phpactor\Extension\Rpc\Response\UpdateFileSourceResponse;
use Phpactor\CodeTransform\Domain\Refactor\ExtractMethod;
use Phpactor\Extension\CodeTransformExtra\Rpc\ExtractMethodHandler;
use Phpactor\Extension\Rpc\Response\Input\TextInput;
use Phpactor\Tests\Unit\Extension\Rpc\HandlerTestCase;

class ExtractMethodHandlerTest extends HandlerTestCase
{
    const SOURCE = '<?php echo "foo";';
    const PATH = '/path/to';
    const OFFSET_START = 1234;
    const OFFSET_END = 1234;
    const METHOD_NAME = 'FOOBAR';

    /**
     * @var ExtractMethod
     */
    private $extractMethod;

    public function setUp(): void
    {
        $this->extractMethod = $this->prophesize(ExtractMethod::class);
    }

    public function createHandler(): Handler
    {
        return new ExtractMethodHandler($this->extractMethod->reveal());
    }

    public function testDemandMethodName(): void
    {
        $action = $this->handle('extract_method', [
            'source' => self::SOURCE,
            'path' => self::PATH,
            'offset_start' => self::OFFSET_START,
            'offset_end' => self::OFFSET_END,
        ]);

        $this->assertInstanceOf(InputCallbackResponse::class, $action);
        $inputs = $action->inputs();
        $this->assertCount(1, $inputs);
        $firstInput = reset($inputs);
        $this->assertEquals(ExtractMethodHandler::NAME, $action->callbackAction()->name());

        $this->assertInstanceOf(TextInput::class, $firstInput);
        $this->assertEquals('method_name', $firstInput->name());
    }

    public function testExtractMethod(): void
    {
        $this->extractMethod->extractMethod(
            self::SOURCE,
            self::OFFSET_START,
            self::OFFSET_END,
            self::METHOD_NAME
        )->willReturn(SourceCode::fromStringAndPath('asd', '/path'));

        $action = $this->handle('extract_method', [
            'source' => self::SOURCE,
            'path' => self::PATH,
            'offset_start' => self::OFFSET_START,
            'offset_end' => self::OFFSET_END,
            'method_name' => self::METHOD_NAME,
        ]);

        $this->assertInstanceof(UpdateFileSourceResponse::class, $action);
    }
}
